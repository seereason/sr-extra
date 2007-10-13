module Extra.Lock
    ( withLock
    , awaitLock
    ) where

import Control.Exception
import Control.Monad.Trans
import Control.Monad.RWS
import System.Directory
import System.IO
import System.Posix.Files
import System.Posix.IO
import System.Posix.Unistd

data LockStatus = Locked | Unlocked Exception

-- | Try to create a lock file containing the PID of this process, if
-- successful perform a task, drop the lock and return the result.  If
-- lock is not obtained, return Nothing.
withLock :: (MonadIO m) => FilePath -> m a -> m (Either Exception a)
withLock path task =
    liftIO checkLock >> liftIO takeLock >>= doTask task >>= liftIO . dropLock
    where
      -- If the lock file exists but the process is gone, break the lock
      checkLock =
          do exists <- doesFileExist path
             case exists of
               True -> liftIO (readFile path) >>= liftIO . processRunning >>= breakLock
               False -> return ()
          where
            breakLock True = return ()
            breakLock False = liftIO (hPutStrLn stderr "Removing stale lock") >> liftIO (removeFile path)
      takeLock =
          -- Try to create the lock file in exclusive mode, if this
          -- succeeds then we have a lock.  Then write the process ID
          -- into the lock and close.
          try (openFd path ReadWrite (Just 0o600) (defaultFileFlags {exclusive = True, trunc = True})) >>=
          either (return . Unlocked)
                 (\ fd -> do h <- fdToHandle fd
                             processID >>= hPutStrLn h >> hClose h >> return Locked)
      doTask task Locked = task >>= return . Right
      doTask _ (Unlocked s) = return (Left s)
      dropLock (Right a) = removeFile path >> return (Right a)
      dropLock x = return x

-- |Like withLock, but instead of giving up immediately, try n times
-- with a wait between each.
awaitLock :: (MonadIO m) => Int -> Int -> FilePath -> m a -> m (Either Exception a)
awaitLock tries usecs path task =
    attempt 0
    where 
      attempt n | n >= tries = return (Left (ErrorCall "Too many failures"))
      attempt n = withLock path task >>= either (\ _ -> liftIO (usleep usecs) >> attempt (n + 1)) (return . Right)

processRunning :: String -> IO Bool
processRunning text = 
    case lines text of
      (pid : _) -> doesDirectoryExist ("/proc/" ++ pid)
      [] -> return False

processID :: IO String
processID = readSymbolicLink "/proc/self"
