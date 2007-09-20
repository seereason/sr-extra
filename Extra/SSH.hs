-- |Functions to manage SSH access.
module Extra.SSH
    ( sshVerify
    , sshExport
    , sshCopy
    ) where

import Control.Monad
import Data.List
import Data.Maybe
import Linspire.Unix.Process
import Network.URI
import System.Cmd
import System.Directory
import System.Environment
import System.IO

-- |Return True if we are able to access user\@host:port via ssh.
sshVerify :: URI -> IO Bool
sshVerify uri =
    case (uriScheme uri, uriAuthority uri) of
      ("ssh:", Just auth) ->
          do let port = case uriPort auth of "" -> "22"; n -> show n
             let dest = uriUserInfo auth ++ uriRegName auth
             result <- lazyCommand ("ssh -o 'PreferredAuthentications hostbased,publickey' -p " ++ port ++ " " ++ dest ++ " pwd") []
             return $ case exitCodeOnly result of
                        [ExitSuccess] -> True
                        _ -> False
      _ -> error $ "Invalid argument to sshVerify: " ++ show uri

-- |Set up the SSH keys to allow access to user\@host:port.  This may
-- prompt for a password on stdin.
sshExport :: URI -> IO ()
sshExport uri =
    case (uriScheme uri, uriAuthority uri) of
      ("ssh:", Just auth) ->
          do let port = case uriPort auth of "" -> "22"; n -> show n
                 dest = uriUserInfo auth ++ uriRegName auth
             user <- getEnv "USER"	-- These will throw exceptions if not set
             home <- getEnv "HOME"
             let pubKey = case user of
                            "root" -> "/root/.ssh/id_rsa.pub"
                            _ -> home ++ "/.ssh/id_rsa.pub"
             exists <- doesFileExist pubKey
             
             case exists of
               False -> do let cmd = "yes '' | ssh-keygen -t rsa"
                           hPutStrLn stderr $ "Creating public key: " ++ cmd
                           result <- system cmd
                           case result of
                             ExitSuccess -> return ()
                             _ -> error $ "Failure: " ++ cmd
               True -> return ()
             result <- lazyCommand ("ssh -o 'PreferredAuthentications hostbased,publickey' -p " ++ port ++ " " ++ dest ++ " pwd") []
             case exitCodeOnly result of
               [ExitSuccess] -> return ()
               _ -> do let cmd = "cat '" ++ pubKey ++ "' | ssh -p " ++ port ++ " " ++ dest ++
                                 " \"mkdir -p .ssh && chmod 700 .ssh && cat >> .ssh/authorized_keys2 && chmod 600 .ssh/authorized_keys2\""
                       hPutStrLn stderr $ "Installing key on remote host: " ++ cmd
                       result <- system cmd
                       case result of
                         ExitSuccess -> return ()
                         _ -> error $ "Failure: " ++ cmd
      _ -> error $ "Invalid argument to sshExport: " ++ show uri

-- |Copy the ssh configuration from $HOME to the \/root directory in a
-- changeroot.
sshCopy :: FilePath -> IO ExitCode
sshCopy root =
    do exists <- doesDirectoryExist "~/.ssh"
       home <- getEnv "HOME"
       case exists of
         True -> system ("rsync -aHxSpDt --delete " ++ home ++ "/.ssh/ " ++ root ++ "/root/.ssh && " ++
                         "chown -R root.root " ++ root ++ "/root/.ssh")
         False -> system "mkdir -p /root/.ssh"
