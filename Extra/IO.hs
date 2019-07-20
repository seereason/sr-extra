{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Extra.IO
    ( -- * IO functions to write files and notice when they change.
      testAndWriteFile
    , writeFileWithBackup
    , findHaskellFiles
    , timeComputation
    ) where

import Control.Exception as E (IOException, throw, try)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO as Text (readFile, writeFile)
import Data.Time (getCurrentTime, diffUTCTime, getCurrentTime, NominalDiffTime)
import Extra.Text (diffText)
import System.Directory (getCurrentDirectory, removeFile, renameFile)
import System.FilePath.Find as Find
    ((==?), (&&?), always, extension, fileType, FileType(RegularFile), find)
import System.IO.Error (isDoesNotExistError)
import System.Log.Logger ({-logM,-} Priority(DEBUG, ERROR))

-- | See if the new Paths code matches the old, if not write it to a
-- file with the suffix ".new" and throw an error so the new code can
-- be inspected and checked in.  If the new file does match, the
-- existing .new file is removed.
testAndWriteFile :: FilePath -> Text -> IO ()
testAndWriteFile dest new = do
  msg1
  removeFileMaybe (dest <> ".new")
  try (Text.readFile dest >>= \old ->
       if old == new
       then msg4 >> pure ()
       else do
         msg5
         Text.writeFile (dest <> ".new") new
         error $ "Generated " <> dest <> ".new does not match existing " <> dest <> ":\n" <>
                  diffText (dest, old) (dest <> ".new", new)) >>=
    either (\(e :: IOException) -> case isDoesNotExistError e of
                                     True -> msg2 >> Text.writeFile dest new
                                     False -> msg3 e >> throw e)
           return
  where
    msg _ s = putStrLn ("Data.Path.Prelude.IO - " <> s)
    msg1 = do
      here <- getCurrentDirectory
      msg DEBUG ("testAndWriteFile " <> show dest <> " " <> take 50 (show new) <> " (cwd=" <> show here <> ")")
    msg2 = msg DEBUG "testAndWriteFile - no existing version"
    msg3 e = msg ERROR ("testAndWriteFile " <> show dest <> " - IOException " ++ show e)
    msg4 = msg DEBUG "testAndWriteFile - match"
    msg5 = msg DEBUG ("testAndWriteFile - mismatch, writing " <> show (dest <> ".new"))

writeFileWithBackup :: FilePath -> Text -> IO ()
writeFileWithBackup dest text = do
  removeFileMaybe (dest <> "~")
  renameFileMaybe dest (dest <> "~")
  removeFileMaybe dest
  Text.writeFile dest text

removeFileMaybe :: FilePath -> IO ()
removeFileMaybe p =
    try (removeFile p) >>=
    either (\(e :: IOException) -> case isDoesNotExistError e of
                                     True -> pure ()
                                     False -> throw e) pure

renameFileMaybe :: FilePath -> FilePath -> IO ()
renameFileMaybe oldpath newpath =
    try (renameFile oldpath newpath) >>=
    either (\(e :: IOException) -> case isDoesNotExistError e of
                                     True -> pure ()
                                     False -> throw e) pure

findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = find always (Find.extension ==? ".hs" &&? fileType ==? RegularFile) dir

-- | Perform an IO operation and return the elapsed time along with the result.
timeComputation :: MonadIO m => m r -> m (r, NominalDiffTime)
timeComputation a = do
  !start <- liftIO getCurrentTime
  !r <- a
  !end <- liftIO getCurrentTime
  return (r, diffUTCTime end start)
