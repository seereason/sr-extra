-- |Some extra operations on files.  The functions here generally
-- return (Right ()) on success, Left [messages] on failure, and throw
-- an exception when a failure leaves things in an inconsistant state.
-- An example of an inconsistant state would be if we got a failure
-- when writing out a file, but were unable to restore the original
-- file to its original position.
module Extra.Files 
    ( getSubDirectories
    , renameAlways
    , renameMissing
    , deleteMaybe
    , installFiles
    , writeAndZipFileWithBackup
    , writeAndZipFile
    , backupFile
    , writeFileIfMissing
    , maybeWriteFile		-- writeFileUnlessSame
    , createSymbolicLinkIfMissing
    , prepareSymbolicLink
    , forceRemoveLink
    ) where

import		 Control.Exception
import		 Control.Monad
import qualified Data.ByteString.Char8 as B
import		 Data.List
import		 Data.Maybe
import		 Extra.Bool
import		 Extra.Misc
import		 Linspire.Unix.Directory
import		 Linspire.Unix.Process
import		 System.Directory
import		 System.IO
import		 System.IO.Error (isDoesNotExistError)
import		 System.Posix.Files

-- | Return the list of subdirectories, omitting . and .. and ignoring
-- symbolic links.
getSubDirectories :: FilePath -> IO [String]
getSubDirectories path =
    getDirectoryContents path >>=
    return . filter (not . (flip elem) [".", ".."]) >>=
    filterM isRealDirectory
    where
      isRealDirectory name = getSymbolicLinkStatus (path ++ "/" ++ name) >>= return . not . isSymbolicLink

-- |Atomically install a list of files.  Returns a list of what went
-- wrong on failure.  Will throw an error if it fails and is unable to
-- restore the original files to their original states.
installFiles :: [(FilePath, FilePath)] -> IO (Either [String] ())
installFiles pairs =
    do backedUp <- mapM (uncurry renameAlways) (zip originalFiles backupFiles)
       case lefts backedUp of
         [] -> 
             do renamed <- mapM (uncurry renameAlways) (zip replacementFiles originalFiles)
                case lefts renamed of
                  [] -> return $ Right ()
                  _ ->
                      -- We failed after all the original files were
                      -- renamed and maybe some of the replacement
                      -- files were installed.  Move all the renamed
                      -- files back into place.
                      do restored <- mapM (uncurry renameAlways) (zip backupFiles originalFiles)
                         case lefts restored of
	                   -- We succeeded in failing.
                           [] -> return . Left . concat . lefts $ renamed
	                   -- Restore failed.  Throw an exception.
                           _ -> error ("installFiles: Couldn't restore original files after write failure:" ++
                                       concat (map message (zip3 replacementFiles originalFiles renamed)) ++
                                       concat (map message (zip3 originalFiles backupFiles restored)))
         _ ->
             -- We failed after renaming all original files, but
             -- before any of the replacement files were installed.
             -- Restore the backup for any missing original files.
             do restored <- mapM (uncurry renameMissing) (zip backupFiles originalFiles)
                case lefts restored of
	          -- We succeeded in failing.
                  [] -> return . Left . concat . lefts $ backedUp
		  -- Restore failed.  Throw an exception.
                  _ -> error ("installFiles: Couldn't restore original files after write failure: " ++
                              concat (map message (zip3 originalFiles backupFiles backedUp)) ++
                              concat (map message (zip3 backupFiles originalFiles restored)))
    where
      replacementFiles = map fst pairs
      originalFiles = map snd pairs
      backupFiles = map (++ "~") originalFiles
      message (path1, path2, Left problems) =
          "\n  " ++ path1 ++ " -> " ++ path2 ++ ": " ++ concat (intersperse ", " (map show problems))
      message (_, _, Right ()) = ""

lefts :: [Either a b] -> [a]
lefts xs = catMaybes $ map (either Just (const Nothing)) xs

-- |Change a file's name only if the new name doesn't exist.
renameMissing :: FilePath -> FilePath -> IO (Either [String] ())
renameMissing old new =
    do exists <- fileExist new
       case exists of
         True -> return $ Right ()
         False -> renameAlways old new

-- |Change a file's name, removing any existing file with the new name.
renameAlways :: FilePath -> FilePath -> IO (Either [String] ())
renameAlways old new =
    do deleted <- deleteMaybe new
       case deleted of
         Right () ->
             try (rename old new) >>=
             return . either (\ e -> Left ["Couldn't rename " ++ old ++ " -> " ++ new ++ ": " ++ show e]) (\ _ -> Right ())
         x -> return x

-- |Change a file's name if it exists.
renameMaybe :: FilePath -> FilePath -> IO (Either [String] ())
renameMaybe old new =
    do exists <- fileExist old
       case exists of
         False -> return $ Right ()
         True -> renameAlways old new

-- |Delete a file if it exists
deleteMaybe :: FilePath -> IO (Either [String] ())
deleteMaybe path =
    do exists <- fileExist path
       case exists of
         False -> return $ Right ()
         True ->
             do status <- getSymbolicLinkStatus path
		-- To do: should we remove the directory contents?
                let rm = if isDirectory status then removeDirectory else removeLink
                try (rm path) >>= return . either (\ e -> Left ["Couldn't remove " ++ path ++ ": " ++ show e]) (const . Right $ ())

-- |Create or update gzipped and bzip2-ed versions of a file.
zipFile :: FilePath -> IO (Either [String] ())
zipFile path =
    do let commands = ["gzip < '" ++ path ++ "' > '" ++ path ++ ".gz'",
                       "bzip2 < '" ++ path ++ "' > '" ++ path ++ ".bz2'"]
       forceRemoveLink (path ++ ".gz")
       forceRemoveLink (path ++ ".bz2")
       results <- mapM (\ cmd -> lazyCommand cmd []) commands
       case filter (/= ExitSuccess) (concat (map exitCodeOnly results)) of
         [] -> return $ Right ()
         _ -> return (Left ["Failure writing and zipping " ++ path ++ ": " ++
                            concat (map writeMessage (zip commands results))])
    where
      writeMessage (command, output) =
          case exitCodeOnly output of
            (ExitFailure n : _) ->
                command ++ " -> " ++ show n ++ ":\n  " ++ B.unpack (B.concat (stderrOnly output))
            _ -> ""

-- |like removeLink, but does not fail if link did not exist
forceRemoveLink :: FilePath -> IO ()
forceRemoveLink fp = removeLink fp `Prelude.catch` (\e -> unless (isDoesNotExistError e) (ioError e))
                 
-- | Write out three versions of a file, regular, gzipped, and bzip2ed.
writeAndZipFileWithBackup :: FilePath -> B.ByteString -> IO (Either [String] ())
writeAndZipFileWithBackup path text =
    backupFile path >>=
    either (\ e -> return (Left ["Failure renaming " ++ path ++ " -> " ++ path ++ "~: " ++ show e]))
           (\ _ -> try (B.writeFile path $! text) >>=
                   either (\ e -> restoreBackup path >>=
                                  either (\ e -> error ("Failed to restore backup: " ++ path ++ "~ -> " ++ path ++ ": " ++ show e))
                                         (\ _ -> return (Left ["Failure writing " ++ path ++ ": " ++ show e])))
                          (\ _ -> zipFile path))

-- | Write out three versions of a file, regular, gzipped, and bzip2ed.
-- This new version assumes the files are written to temporary locations,
-- so any existing file there can be removed.
writeAndZipFile :: FilePath -> B.ByteString -> IO (Either [String] ())
writeAndZipFile path text =
    deleteMaybe path >>=
    either (\ e -> return (Left ["Failure removing " ++ path ++ ": " ++ show e]))
           (\ _ -> try (B.writeFile path $! text) >>=
                   either (\ e -> return (Left ["Failure writing " ++ path ++ ": " ++ show e]))
                          (\ _ -> zipFile path))

-- Turn a file into a backup file if it exists.
backupFile :: FilePath -> IO (Either [String] ())
backupFile path = renameMaybe path (path ++ "~")

restoreBackup :: FilePath -> IO (Either [String] ())
restoreBackup path = renameMaybe (path ++ "~") path

-- | Like writeFile, but if the file already exists don't touch it.
-- Example: writeFileIfMissing True \"\/var\/lib\/dpkg\/status\" \"\"
writeFileIfMissing :: Bool -> FilePath -> String -> IO ()
writeFileIfMissing mkdirs path text =
    do
      exists <- doesFileExist path
      case exists of
        False ->
            do
              if mkdirs then
                  createDirectoryIfMissing True (parentPath path) else
                  return ()
              writeFile path $! text
        True ->
            return ()

-- | Write a file if its content is different from the given text.
maybeWriteFile :: FilePath -> String -> IO ()
maybeWriteFile path text =
    doesFileExist path >>= cond (checkFile path text) (writeFile path $! text)
    where
      checkFile path text = readFile path >>=
                            return . (== text) >>=
                            cond (return ()) (removeFile path >> writeFile path $! text)

-- |Add-on for System.Posix.Files
createSymbolicLinkIfMissing :: String -> FilePath -> IO ()
createSymbolicLinkIfMissing text path =
    do
      -- My.ePut ("createSymbolicLinkIfMissing \"" ++ text ++ "\" \"" ++ path ++ "\"")
      result <- try $ getSymbolicLinkStatus path
      either (\ _ -> createSymbolicLink text path) (\ _ -> return ()) result

prepareSymbolicLink :: FilePath -> FilePath -> IO ()
prepareSymbolicLink name path =
    checkExists >>= checkType >>= checkContent
    where
      checkExists = doesDirectoryExist path >>= orCreate
      checkType False = return False
      checkType True = getSymbolicLinkStatus path >>= return . isSymbolicLink >>= orReplace
      checkContent False = return ()
      checkContent True = readSymbolicLink path >>= return . (== name) >>= orReplace >> return ()
      orReplace True = return True
      orReplace False = do removeRecursiveSafely path; orCreate False
      orCreate True = return True
      orCreate False = do createSymbolicLink name path; return False
