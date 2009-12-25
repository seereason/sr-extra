module Extra.Lock
    ( withLock
    , awaitLock
    ) where

import Control.OldException
import Control.Monad.RWS
import System.Directory
import System.IO
import System.IO.Error hiding (try)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Unistd

withLock :: (MonadIO m) => FilePath -> m a -> m (Either Exception a)
withLock path task =
    liftIO checkLock >>= liftIO . takeLock >>= doTask task >>= liftIO . dropLock
    where
      -- Return True if file is locked by a running process, false otherwise
      checkLock :: IO (Either Exception ())
      checkLock = try (readFile path) >>= either (return . checkReadError) (processRunning . lines)
      checkReadError (IOException e) | isDoesNotExistError e = (Right ())
      checkReadError e = Left e
      processRunning (pid : _) =
          do exists <- doesDirectoryExist ("/proc/" ++ pid)
             case exists of
               True -> return (Left (lockedBy pid))
               False -> breakLock
      processRunning [] = breakLock
      lockedBy pid = IOException (mkIOError alreadyInUseErrorType ("Locked by " ++ pid) Nothing (Just path))
      breakLock = do try (removeFile path) >>= return . either checkBreakError (const (Right ()))
      checkBreakError (IOException e) | isDoesNotExistError e = (Right ())
      checkBreakError e = Left e
      takeLock :: Either Exception () -> IO (Either Exception ())
      takeLock (Right ()) =
          -- Try to create the lock file in exclusive mode, if this
          -- succeeds then we have a lock.  Then write the process ID
          -- into the lock and close.
          try (openFd path ReadWrite (Just 0o600) (defaultFileFlags {exclusive = True, trunc = True})) >>=
          either (return . Left)
                 (\ fd -> do h <- fdToHandle fd
                             processID >>= hPutStrLn h >> hClose h >> return (Right ()))
      takeLock (Left e) = return (Left e)
      doTask task (Right ()) = task >>= return . Right
      doTask _ (Left e) = return (Left e)
      dropLock (Right a) = try (removeFile path) >>= return . checkDrop a
      dropLock (Left e) = return (Left e)
      checkDrop a (Right ()) = Right a
      checkDrop a (Left (IOException e)) | isDoesNotExistError e = Right a
      checkDrop _ (Left e) = Left e

-- |Like withLock, but instead of giving up immediately, try n times
-- with a wait between each.
awaitLock :: (MonadIO m) => Int -> Int -> FilePath -> m a -> m (Either Exception a)
awaitLock tries usecs path task =
    attempt 0
    where 
      attempt n | n >= tries = return (Left (ErrorCall "Too many failures"))
      attempt n = withLock path task >>= either (\ _ -> liftIO (usleep usecs) >> attempt (n + 1)) (return . Right)

processID :: IO String
processID = readSymbolicLink "/proc/self"
