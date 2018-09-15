{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      System.Process.EntryPoint
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module System.Process.EntryPoint
    (
    -- * Entry Point
    --
    -- Combines functionality of 'forkProcessIfPid1' and 'daemonise' into one
    -- process entry point.
      entryPoint

    , EntryPointOptions(..)
    , defaultEntryPointOptions

    , WorkingDirectory(..)
    , setWorkingDirectory

    , Application(..)
    , applicationFromReader

    , dropPrivileges

    -- * PID 1
    --
    -- $pid1
    , forkProcessIfPid1

    -- * Daemonise
    , daemonise
    )
  where

import Prelude ((*), (+), error, fromIntegral)

import Control.Applicative (pure, (*>))
import Control.Concurrent
    ( forkIO
    , newEmptyMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    )
import Control.Exception (assert, bracket, catch, throwIO)
import Control.Monad ((>=>), (>>=), forever, unless)
import Data.Bool (Bool(False, True), otherwise)
import Data.Eq ((==))
import Data.Foldable (for_, mapM_)
import Data.Function (($), (.), const)
import Data.Functor (void)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (String)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.IO (FilePath, IO)
import System.IO.Error (isDoesNotExistError)

import System.Directory (getHomeDirectory, setCurrentDirectory)
import System.Posix
    ( CPid
    , Handler(Catch, Ignore)
    , OpenMode(ReadWrite)
    , ProcessStatus(Exited, Stopped, Terminated)
    , Signal
    , changeWorkingDirectory
    , closeFd
    , createSession
    , defaultFileFlags
    , dupTo
    , exitImmediately
    , forkProcess
    , getAnyProcessStatus
    , getGroupEntryForName
    , getProcessID
    , getUserEntryForName
    , groupID
    , installHandler
    , openFd
    , setFileCreationMask
    , setGroupID
    , setUserID
    , sigHUP
    , sigINT
    , sigKILL
    , sigTERM
    , signalProcess
    , stdError
    , stdInput
    , stdOutput
    , userID
    )
import Control.Monad.Trans.Reader (ReaderT(ReaderT))
import System.Lumberjack.Backend (SomeLoggingBackend, noLoggingBackend)
import qualified System.Lumberjack.Backend as LoggingBackend (close)


data WorkingDirectory
    = StayInCurrentDirectory
    -- ^ Do not change working directory.
    | HomeDirectory
    -- ^ Switch to home directory of current user.
    | OtherDirectory FilePath
    -- ^ Change working directory to provided path.

data EntryPointOptions a = EntryPointOptions
    { name :: Maybe String
    -- ^ Daemon name used as a PID file name.

    , user :: Maybe String
    , group :: Maybe String
    , workingDirectory :: WorkingDirectory
    , exitTimeoutInSeconds :: Word
    , pidfileDirectory :: FilePath

    , startLogger :: IO SomeLoggingBackend

    , acquirePrivilegedResource :: SomeLoggingBackend -> IO a
    , releasePrivilegedResource :: SomeLoggingBackend -> a -> IO ()

    , runInForeground :: Bool
    -- ^ Force application to run in foreground even if it's running in
    -- container. Useful for debugging and testing.

    -- TODO: Retry policy for cases when the application dies.
    }
  deriving (Generic)

defaultEntryPointOptions :: EntryPointOptions ()
defaultEntryPointOptions = EntryPointOptions
    { name = Nothing
    , user = Nothing
    , group = Nothing
    , workingDirectory = StayInCurrentDirectory
    , exitTimeoutInSeconds = 5
    , pidfileDirectory = "/var/run"

    , startLogger = pure noLoggingBackend

    , acquirePrivilegedResource = const (pure ())
    , releasePrivilegedResource = \_ _ -> pure ()

    , runInForeground = False
    }

newtype Application a =
    Application (SomeLoggingBackend -> a -> IO ())

applicationFromReader
    :: (SomeLoggingBackend -> a -> IO r)
    -> ReaderT r IO ()
    -> Application a
applicationFromReader toEnvironment (ReaderT action) =
    Application $ \logger params ->
        toEnvironment logger params >>= action

entryPoint :: EntryPointOptions a -> Application a -> IO ()
entryPoint opts@EntryPointOptions{..} app =
    withLogger $ \logger ->
        entryPoint' (runApplication logger opts app)
  where
    entryPoint' realApp
      | runInForeground = realApp
      | otherwise = do
        myID <- getProcessID
        if myID == 1
            then pid1ForkProcess exitTimeoutInSeconds realApp
            else daemonise realApp

    withLogger = bracket startLogger LoggingBackend.close

runApplication :: SomeLoggingBackend -> EntryPointOptions a -> Application a -> IO ()
runApplication logger EntryPointOptions{..} (Application app) =
    withResource $ \resource -> do
        dropPrivileges user group
        -- We need to dropPrivileges (potentially swithc user) before
        -- changing working directory to use the correct value in case of
        -- 'HomeDirectory'.
        setWorkingDirectory workingDirectory

        app logger resource
  where
    withResource =
        acquirePrivilegedResource logger
            `bracket` releasePrivilegedResource logger

dropPrivileges
    :: Maybe String
    -- ^ Set user
    -> Maybe String
    -- ^ Set group
    -> IO ()
dropPrivileges user group = do
    onJust group (getGroupEntryForName >=> setGroupID . groupID)
    onJust user (getUserEntryForName >=> setUserID . userID)
  where
    onJust = for_

setWorkingDirectory :: WorkingDirectory -> IO ()
setWorkingDirectory = \case
    StayInCurrentDirectory -> pure ()
    HomeDirectory -> getHomeDirectory >>= setCurrentDirectory
    OtherDirectory path -> setCurrentDirectory path

-- {{{ PID 1 ------------------------------------------------------------------
--
-- A lot of this code was either inspired or taken from `pid1` package authored
-- by Michael Snoyman and released under MIT license.
--
-- Version from which this was copied:
-- <http://hackage.haskell.org/package/pid1-0.1.2.0>
--
-- Unfortunately the original package doesn't expose low-level functionality,
-- which is the reason for the copy-paste.

-- | This function will check if the current process has a process ID of 1. If
-- it does, it will install signal handlers for @SIGTERM@ and @SIGINT@, set up
-- a loop to reap all orphans, spawn a child process, and when that child dies,
-- kill all other processes (first with a @SIGTERM@ and then a @SIGKILL@) and
-- exit with the child's exit code.
--
-- If this process does not have PID 1, then it will simply start provided
-- action.
--
-- This function will never exit: it will always terminate your process, unless
-- some exception is thrown.
forkProcessIfPid1
    :: Word
    -- ^ Timeout in seconds between @SIGTERM@ and @SIGKILL@ when reaping
    -- children.
    -> IO ()
    -- ^ Action to run, either in a child process, if started as PID 1, or
    -- directly.
    -> IO ()
forkProcessIfPid1 timeout action = do
    myPid <- getProcessID
    if myPid == 1
        then pid1ForkProcess timeout action
        else action

pid1ForkProcess :: Word -> IO () -> IO a
pid1ForkProcess timeout action = do
    killChildren <- newEmptyMVar

    _ <- forkIO $ do
        takeMVar killChildren
        killAllChildren timeout

    let startKillingChildren = void $ tryPutMVar killChildren ()
    _ <- installHandler sigTERM (Catch startKillingChildren) Nothing
    _ <- installHandler sigINT  (Catch startKillingChildren) Nothing

    child <- forkProcess action
    reaper startKillingChildren child

killAllChildren :: Word -> IO ()
killAllChildren timeout = do
    -- Value `-1` means send signal to all children.
    signalProcess sigTERM (-1) `catch` \e ->
        unless (isDoesNotExistError e) $ throwIO e

    -- Wait for `timeout` seconds. We don't need to put in any logic about
    -- whether there are still child processes; if all children have
    -- exited, then the reap loop will exit and our process will shut
    -- down.
    threadDelay $ fromIntegral (timeout * 1000 * 1000)

    -- OK, some children didn't exit. Now time to get serious!
    signalProcess sigKILL (-1) `catch` \e ->
        unless (isDoesNotExistError e) $ throwIO e

reaper :: IO () -> CPid -> IO a
reaper startKilling child = do
    -- Track the ProcessStatus of the child
    childStatus <- newEmptyMVar

    -- Keep reaping one child. Eventually, when all children are dead,
    -- we'll get an exception. We catch that exception and, assuming
    -- it's the DoesNotExistError we're expecting, know that all
    -- children are dead and exit.
    forever (reapOne childStatus) `catch` \e ->
        if isDoesNotExistError e
            -- no more child processes
            then do
                takeMVar childStatus >>= exitImmediately . toExitCode
                error "reaper: This can never be reached"

            -- some other exception occurred, reraise it
            else throwIO e
  where
    reapOne childStatus =
        -- Block until a child process exits
        getAnyProcessStatus True False >>= \case
            -- This should never happen, if there are no more child
            -- processes we'll get an exception instead
            Nothing -> assert False (pure ())
            -- Got a new dead child. If it's the child we created in
            -- main, then start killing all other children. Otherwise,
            -- we're just reaping.
            Just (pid, status)
                | pid == child -> do
                    -- Take the first status of the child. It's possible -
                    -- however unlikely - that the process ID could end up
                    -- getting reused and there will be another child exiting
                    -- with the same PID. Just ignore that.
                    void $ tryPutMVar childStatus status
                    startKilling
                | otherwise -> pure ()

-- | Convert a ProcessStatus to an ExitCode. In the case of a signal being the
-- cause of termination, see 'signalToEC'.
toExitCode :: ProcessStatus -> ExitCode
toExitCode (Exited ec) = ec
#if MIN_VERSION_unix(2, 7, 0)
toExitCode (Terminated sig _) = signalToEC sig
#else
toExitCode (Terminated sig) = signalToEC sig
#endif
toExitCode (Stopped sig) = signalToEC sig

-- | Follow the convention of converting a signal into an exit code by adding
-- 128.
signalToEC :: Signal -> ExitCode
signalToEC sig = ExitFailure (fromIntegral sig + 128)

-- $pid1
--
-- PID 1 is Unix @init@ process. This usually means that we are running in a
-- container as the only process.
--
-- Useful reading:
--
-- * [Wikipedia: Init](https://en.wikipedia.org/wiki/Init)
-- * <https://www.fpcomplete.com/blog/2016/10/docker-demons-pid1-orphans-zombies-signals>

-- }}} PID 1 ------------------------------------------------------------------

-- {{{ Daemonise --------------------------------------------------------------
--
-- Code was copied from `hdaemonize` package authored by Anton Tayanovskyy, and
-- Fred Ross <http://hackage.haskell.org/package/hdaemonize-0.5.5>. It is
-- released under BSD 3-Clause license.
--
-- Reason for copying is that `hdaemonize` has additional dependencies that
-- aren't useful to us.

-- | Turning a process into a daemon involves a fixed set of operations on unix
-- systems, described in section 13.3 of Stevens and Rago, "Advanced
-- Programming in the Unix Environment."  Since they are fixed, they can be
-- written as a single function, 'daemonise' taking an 'IO' action which
-- represents the daemon's actual activity.
--
-- Briefly, 'daemonise' sets the file creation mask to 0, forks twice, changed
-- the working directory to @/@, closes @stdin@, @stdout@, and @stderr@, blocks
-- 'sigHUP', and runs its argument.  Strictly, it should close all open file
-- descriptors, but this is not possible in a sensible way in Haskell.
--
-- The most trivial daemon would be
--
-- > daemonize (forever $ return ())
--
-- which does nothing until killed.
daemonise :: IO () -> IO ()
daemonise program = do
    _ <- setFileCreationMask 0
    _ <- forkProcess p
    exitImmediately ExitSuccess
  where
    p  = do
        _ <- createSession
        _ <- forkProcess p'
        exitImmediately ExitSuccess

    p' = do
        changeWorkingDirectory "/"
        closeFileDescriptors
        blockSignal sigHUP
        program

closeFileDescriptors :: IO ()
closeFileDescriptors = do
    null <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
    mapM_ (\fd -> closeFd fd *> dupTo null fd) [stdInput, stdOutput, stdError]

blockSignal :: Signal -> IO ()
blockSignal sig = void (installHandler sig Ignore Nothing)

-- }}} Daemonise --------------------------------------------------------------
