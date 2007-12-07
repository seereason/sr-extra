module Extra.SSH
    ( sshVerify
    , sshExport
    ) where

import Control.Monad(unless)
import System.Cmd
import System.Directory
import System.Posix.User
import System.Posix.Files
import System.Environment
import System.Exit
import System.IO
import Network.URI
import GHC.Read(readEither)

sshExport :: String -> Maybe Int -> IO (Either String ())
sshExport dest port =
    generatePublicKey >>=
    either (return . Left) (testAccess dest port) >>=
    either (return . Left) (openAccess dest port)

-- parseURI "ssh://dsf@server:22"
-- URI {uriScheme = "ssh:", uriAuthority = Just (URIAuth {uriUserInfo = "dsf@", uriRegName = "server", uriPort = ":22"}), uriPath = "", uriQuery = "", uriFragment = ""}

-- |Make sure there is a public key for the local account
generatePublicKey :: IO (Either String FilePath)
generatePublicKey =
    do user <- getEnv "USER"
       home <- getUserEntryForName user >>= return . homeDirectory
       let keypath = home ++ "/.ssh/id_rsa.pub"
       exists <- doesFileExist keypath
       case exists of
         True -> return . Right $ keypath
         False ->
             do hPutStrLn stderr $ "generatePublicKey " ++ " -> " ++ keypath
                result <- system cmd
                case result of
                  ExitSuccess -> return . Right $ keypath
                  ExitFailure n -> return . Left $ "Failure: " ++ show cmd ++ " -> " ++ show n
    where cmd = "yes '' | ssh-keygen -t rsa; fi"

-- |See if we already have access to the account.
sshVerify :: String -> Maybe Int -> IO Bool
sshVerify dest port =
    do result <- system (sshTestCmd dest port)
       return $ case result of
                  ExitSuccess -> True		-- We do
                  ExitFailure _ -> False	-- We do not
    where
      sshTestCmd dest port =
          ("ssh -o 'PreferredAuthentications hostbased,publickey' " ++
           (maybe "" (("-p " ++) . show) port) ++ " " ++ show dest ++ " pwd > /dev/null && exit 0")

testAccess :: String -> Maybe Int -> FilePath -> IO (Either String (Maybe FilePath))
testAccess dest port keypath =
    do flag <- sshVerify dest port
       case flag of
         True -> return . Right $ Nothing
         False -> return . Right . Just $ keypath

-- |Try to set up the keys so we have access to the account
openAccess :: String -> Maybe Int -> Maybe FilePath -> IO (Either String ())
openAccess _ _ Nothing = return . Right $ ()
openAccess dest port (Just keypath) =
    do hPutStrLn stderr $ "openAccess " ++ show dest ++ " " ++ show port ++ " " ++ show keypath
       let cmd = sshOpenCmd dest port keypath
       result <- system cmd
       case result of
         ExitSuccess -> return . Right $ ()
         ExitFailure n -> return . Left $ "Failure: " ++ show cmd ++ " -> " ++ show n
    where
      sshOpenCmd dest port keypath =
          "cat " ++ keypath ++ " | " ++ "ssh " ++ (maybe "" ((++ "-p ") . show) port) ++ " " ++ show dest ++ " '" ++ sshOpenRemoteCmd ++ "'"
      sshOpenRemoteCmd =
          ("chmod g-w . && " ++				-- Ssh will not work if the permissions aren't just so
           "chmod o-rwx . && " ++
           "mkdir -p .ssh && " ++
           "chmod 700 .ssh && " ++
           "cat >> .ssh/authorized_keys2 && " ++	-- Add the key to the authorized key list
           "chmod 600 .ssh/authorized_keys2")

-- This used to be main.
test =
    getDest >>=
    either (return . Left) (uncurry sshExport) >>=
    either (error . show) (const . exitWith $ ExitSuccess)

-- |Get the destination account info from the command line
getDest :: IO (Either String (String, Maybe Int))
getDest =
    getArgs >>= checkArgs
    where checkArgs [dest] =
              return $ case parseURI ("ssh://" ++ dest) of
                         Just (URI {uriAuthority = Just (URIAuth {uriUserInfo = user, uriRegName = host, uriPort = ""})}) ->
                             Right (user ++ host, Nothing)
                         Just (URI {uriAuthority = Just (URIAuth {uriUserInfo = user, uriRegName = host, uriPort = port})}) ->
                             case readEither (dropWhile (== ':') port) :: Either String Int of
                               Left s -> Left $ "Invalid destination: " ++ dest ++ " (" ++ s ++ ")"
                               Right n -> Right (user ++ host, Just n)
                         _ -> Left $ "Invalid destination: " ++ dest
          checkArgs args = return . Left $ "Usage: sshexport user@host"
