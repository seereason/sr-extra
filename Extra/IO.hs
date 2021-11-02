{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module Extra.IO
    ( -- * IO functions to write files and notice when they change.
      testAndWriteDotNew
    , testAndWriteBackup
    , testAndWrite
    , writeFileWithBackup
    , findHaskellFiles
    , timeComputation
    ) where

import Control.Exception as E (IOException, throw, try)
import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Monoid ((<>))
import Data.Text as Text (length, take, Text)
import Data.Text.IO as Text (readFile, writeFile)
import Data.Time (getCurrentTime, diffUTCTime, getCurrentTime, NominalDiffTime)
--import Extra.Log (alog)
import Extra.Text (diffText)
import System.Directory (getCurrentDirectory, removeFile, renameFile)
import System.FilePath.Find as Find
    ((==?), (&&?), always, extension, fileType, FileType(RegularFile), find)
import System.IO.Error (isDoesNotExistError)
import System.Log.Logger (logM, Priority(DEBUG, ERROR))

testAndWriteDotNew :: FilePath -> Text -> IO ()
testAndWriteDotNew dest new = testAndWrite writeDotNew dest new

testAndWriteBackup :: FilePath -> Text -> IO ()
testAndWriteBackup dest new = testAndWrite (\dest' _ new' -> writeFileWithBackup dest' new') dest new

-- | See if the new Paths code matches the old, if not write it to a
-- file with the suffix ".new" and throw an error so the new code can
-- be inspected and checked in.  If the new file does match, the
-- existing .new file is removed.
testAndWrite :: (FilePath -> Text -> Text -> IO ()) -> FilePath -> Text -> IO ()
testAndWrite changeAction dest new = do
  here <- getCurrentDirectory
  logM "Extra.IO" DEBUG ("testAndWrite " <> show dest <> " " <> show (shorten 50 new) <> " (cwd=" <> show here <> ")")
  removeFileMaybe (dest <> ".new")
  try (Text.readFile dest >>= \old ->
       when (old /= new) (changeAction dest old new)) >>=
    either (\(e :: IOException) ->
              case isDoesNotExistError e of
                True -> do
                  logM "Extra.IO" DEBUG "testAndWrite - no existing version"
                  Text.writeFile dest new
                False -> do
                  logM "Extra.IO" ERROR ("testAndWrite " <> show dest <> " - IOException " ++ show e)
                  throw e)
           return

-- | Shorten a string to a maximum length by replacing its suffix with "..."
shorten :: Int -> Text -> Text
shorten n t | n <= 3 = Text.take n t -- no room for an ellipsis
shorten n t | Text.length t > n - 3 = Text.take (n - 3) t <> "..."
shorten _ t = t

-- | If the new file does not match the old, write it to file.new and error.
writeDotNew :: FilePath -> Text -> Text -> IO ()
writeDotNew dest old new = do
  logM "Extra.IO" DEBUG ("writeDotNew - mismatch, writing " <> show (dest <> ".new"))
  Text.writeFile (dest <> ".new") new
  error ("Generated " <> dest <> ".new does not match existing " <> dest <> ":\n" <>
         diffText (dest, old) (dest <> ".new", new) <>
         "\nIf these changes look reasonable move " <> dest <> ".new to " <> dest <> " and retry.")


-- | Rename existing file with suffix "~" and write a new file
writeFileWithBackup :: FilePath -> Text -> IO ()
writeFileWithBackup dest text = do
  removeFileMaybe (dest <> "~")
  renameFileMaybe dest (dest <> "~")
  removeFileMaybe dest
  Text.writeFile dest text

-- | Remove a file if it exists
removeFileMaybe :: FilePath -> IO ()
removeFileMaybe p =
    try (removeFile p) >>=
    either (\(e :: IOException) -> case isDoesNotExistError e of
                                     True -> pure ()
                                     False -> throw e) pure

-- | Rename a file if it exists
renameFileMaybe :: FilePath -> FilePath -> IO ()
renameFileMaybe oldpath newpath =
    try (renameFile oldpath newpath) >>=
    either (\(e :: IOException) -> case isDoesNotExistError e of
                                     True -> pure ()
                                     False -> throw e) pure

-- | Find all regular files with extension .hs
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = find always (Find.extension ==? ".hs" &&? fileType ==? RegularFile) dir

-- | Perform an IO operation and return the elapsed time along with the result.
timeComputation :: MonadIO m => m r -> m (r, NominalDiffTime)
timeComputation a = do
  !start <- liftIO getCurrentTime
  !r <- a
  !end <- liftIO getCurrentTime
  return (r, diffUTCTime end start)
