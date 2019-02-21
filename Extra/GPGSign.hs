module Extra.GPGSign
    ( sign
    , PGPKey(..)
    , pgpSignFiles
    , pgpSignFile
    , cd
    ) where

import System.Process
import System.IO
import System.Exit
import Extra.Misc

_test :: PGPKey'' -> [FilePath] -> IO [FilePath]
_test key files =
    mapM (sign key) files

type PGPKey'' = String


sign :: PGPKey'' -> FilePath -> IO FilePath
sign keyname path =
    do (_, _,err,pid) <- runInteractiveProcess cmd args workingDir Nothing
       status <- waitForProcess pid
       case status of
         ExitSuccess -> return outputPath
         ExitFailure _ ->
             do gpgerr <- hGetContents err
                hPutStr stderr gpgerr
                exitWith status
       where
         cmd = "/usr/bin/gpg"
         args = [ "--batch"
                , "--yes"
                , "--default-key", keyname
                , "-o", outputPath
                , "--clearsign"
                , path
                ]
         outputPath = path ++ ".gpg"
         workingDir = Nothing -- Just (dirName path)

data PGPKey = Key String | Default deriving Show

pgpSignFiles :: FilePath -> PGPKey -> [FilePath] -> IO [Bool]
pgpSignFiles root key files = cd root $ mapM (pgpSignFile key) files

pgpSignFile :: PGPKey -> FilePath -> IO Bool
pgpSignFile keyname path =
    do (_, _,err,pid) <- runInteractiveProcess cmd args workingDir Nothing
       status <- waitForProcess pid
       case status of
         ExitSuccess -> return True
         ExitFailure _code ->
             do gpgerr <- hGetContents err
                hPutStr stderr gpgerr
                return False
       where
         cmd = "/usr/bin/gpg"
         args = defaultKey ++
             [ "--batch"
             , "--yes"
             , "-o", outputPath
             , "--armor"
             , "--detach-sign"
             , path
             ]
         defaultKey = case keyname of Key name -> ["--default-key", name]; Default -> []
         outputPath = path ++ ".gpg"
         workingDir = Nothing -- Just (dirName path)
