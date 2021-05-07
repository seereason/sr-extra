{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Extra.Build where

import Control.Concurrent.Async
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.State (MonadIO(liftIO))
import Control.Exception.Enclosed (catchAny)
import qualified Data.ByteString.Lazy as BS (fromStrict, readFile)
import Data.Char
import Data.Digest.Pure.MD5 (md5)
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.ListLike (fromStringLike, isPrefixOf, putStr, putStrLn, show)
import Data.ListLike.IO (ListLikeIO)
import Data.ListLike.String (StringLike, unwords)
import Data.Text as Text
   (drop, dropEnd, dropWhileEnd, intercalate, isInfixOf,
    length, lines, null, pack, stripPrefix, take, Text, unlines, unpack, words)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Extra.IO (testAndWriteBackup)
import GHC.Exts (Item)
import Prelude hiding (putStrLn, show)
import Shelly (asyncSh)
import Shelly.Lifted as Shelly hiding (command, run, run_, FilePath)
import qualified Shelly.Lifted as Shelly (run, run_, FilePath)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stderr, stdout, )
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Text (Parser)

default (Text)

-- Linking /home/dsf/git/happstack-ghcjs.foo/dist-newstyle/build/x86_64-linux/ghc-8.6.5/happstack-ghcjs-server-0.1.0.9/x/appraisalscribe3-server/build/appraisalscribe3-server/appraisalscribe3-server ...

putStrLn' :: (ListLikeIO s (Item s), StringLike s, MonadIO m) => s -> m ()
putStrLn' = liftIO . Data.ListLike.putStrLn

putStr' :: (ListLikeIO s (Item s), StringLike s, MonadIO m) => s -> m ()
putStr' = liftIO . Data.ListLike.putStr

run :: (MonadIO m, MonadSh m) => Shelly.FilePath -> [Text] -> m Text
run c args = putStr' " run> " >> Shelly.run c args

run_ :: (MonadIO m, MonadSh m) => Shelly.FilePath -> [Text] -> m ()
run_ c args = putStr' " run_> " >> Shelly.run_ c args

shelly' :: Sh a -> IO a
shelly' = shelly
  . tracing False -- do not reprint the list of commands after exiting
  . print_commands True
  . print_stderr True
  . print_stdout True
  -- Indent command output.
  . log_stdout_with out
  . log_stderr_with err

out :: Text -> IO ()
out = TIO.hPutStrLn stdout . ("  1> " <>)

-- Do not prefix lines that emacs needs to recognize as error messages
err :: Text -> IO ()
err s = TIO.hPutStrLn stderr $
  case parse errorLocP "message" s of
    Right ErrorLoc -> s
    Right WarningLoc -> s
    Right (BoundHere loc) -> loc <> ": bound here"
    Right (DefinedHere loc) -> loc <> ": defined here"
    Right (ImportedHere loc) -> loc <> ": imported here"
    _ ->  "  2> " <> s
  where

data MessageType
  = ErrorLoc
  | WarningLoc
  | BoundHere Text
  | ImportedHere Text
  | DefinedHere Text
  | Other

-- > parse errorLocP "message" "build.hs:77:3: error:"
-- Right ()
errorLocP :: Parser MessageType
errorLocP = try boundHereP <|> try importedHereP <|> try definedHereP <|> try atHereP <|> try errorP <|> try warningP <|> otherP

boundHereP :: Parser MessageType
boundHereP = do
  spaces
  string "bound at"
  spaces
  loc <- locationP
  pure (BoundHere loc)

atHereP :: Parser MessageType
atHereP = do
  spaces
  string "at"
  spaces
  loc <- locationP
  pure (BoundHere loc)

definedHereP :: Parser MessageType
definedHereP = do
  spaces
  string "defined at"
  spaces
  loc <- locationP
  pure (DefinedHere loc)

importedHereP :: Parser MessageType
importedHereP = do
  spaces
  string "imported from ‘"
  many1 pathP
  string "’ at "
  loc <- locationP
  pure (ImportedHere loc)

locationP :: Parser Text
locationP = do
  path <- pathP
  char ':'
  lno <- many1 digit
  char ':'
  cno <- many1 digit
  pure (pack (path <> ":" <> lno <> ":" <> cno))

pathP = many1 (alphaNum <|> oneOf "/._")

errorP :: Parser MessageType
errorP = do
  locationP
  string ": error:"
  pure ErrorLoc

warningP :: Parser MessageType
warningP = do
  locationP
  string ": warning:"
  pure WarningLoc

otherP :: Parser MessageType
otherP = pure Other

setBuffering :: IO ()
setBuffering = do
  -- We don't want to switch between stdout and stderr mid line.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

linkMessagePrefix :: IO Text
linkMessagePrefix = ((\t -> "Linking " <> t <> "/") . Text.pack) <$> getCurrentDirectory

sumpath :: String
sumpath = "client.sha256"

writeClientBinDir :: Shelly.FilePath -> Text -> Sh ()
writeClientBinDir dir path =
  liftIO $ testAndWriteBackup (dir </> "ClientBinDir.hs")
    (Text.unlines
       ["-- This module is written by the build.hs script if it is absent.  If it is present its content",
        "-- is validated and if there is a change the script will fail.  In that case the new version must",
        "-- be copied into place (and checked into VC.)  Furthermore, the NIX build has a different mechanism",
        "-- for creating ClientBinDir.hs which can be found in seereason-private.nix.",
        "--",
        "{-# LANGUAGE TemplateHaskell #-}",
        "module ClientBinDir where",
        "import Data.FileEmbed (embedFile)",
        "import Language.Haskell.TH.Syntax (addDependentFile)",
        "clientBinDir :: FilePath -> IO FilePath",
        "clientBinDir _ =",
        "  let _sum = $(addDependentFile " <> pack (show ("../" <> sumpath)) <> " >> embedFile " <> pack (show ("../" <> sumpath)) <> ") in",
        "  pure " <> pack (show ("../" <> unpack path <> "/.."))])

-- Copy of the ProgramVersion type in Base.ProgramVersion.
-- This could go into a separate library.
data ProgramVersion =
  ProgramVersion
  { _gitCommit :: Text
  , _gitAuthorDate :: Text -- UTCTime
  -- ^ I was going to store this as UTCTime but some conversion issues
  -- arose in ghcjs.  This should be investigated.
  , _localChanges :: Text
  } deriving (Read, Show, Eq, Ord)

writeServerVersion :: Shelly.FilePath -> Sh ()
writeServerVersion dir = do
  diff <- silently $ run "git" ["diff", "--", "."]
  when (diff /= "") $ do
    let sig = Text.take 8 . show . md5 . BS.fromStrict . encodeUtf8 $ diff
    liftIO $ putStrLn ("server diff sig: " <> sig)
  v <- (read . unpack) <$> run "git" ["log", "-1", "--pretty=format:ProgramVersion {_gitCommit=\"%H\", _gitAuthorDate=\"%ai\", _localChanges=\"\"}"]
  let v' = v {_localChanges = diff}
  liftIO $ testAndWriteBackup (dir </> "ServerVersion.hs") $
    (Text.unlines ["module ServerVersion where",
                "import Base.ProgramVersion",
                "serverVersion :: ProgramVersion",
                "serverVersion = " <> pack (show v')])

writeClientVersion :: Shelly.FilePath -> Sh ()
writeClientVersion dir = do
  diff <- silently $ run "git" ["diff", "--", "."]
  when (diff /= "") $ do
    let sig = Text.take 8 . show . md5 . BS.fromStrict . encodeUtf8 $ diff
    liftIO $ putStrLn ("client diff sig: " <> sig)
  v <- (read . unpack) <$> run "git" ["log", "-1", "--pretty=format:ProgramVersion {_gitCommit=\"%H\", _gitAuthorDate=\"%ai\", _localChanges=\"\"}"]
  let v' = v {_localChanges = diff}
  liftIO $ testAndWriteBackup (dir </> "ClientVersion.hs") $
    (Text.unlines ["module ClientVersion where",
                "import Base.ProgramVersion",
                "clientVersion :: ProgramVersion",
                "clientVersion = " <> pack (show v')])

cabal :: [Text] -> Sh ()
cabal = void . run "cabal"

build :: [Text] -> Sh ()
build args = cabal ("new-build" : args)

clean :: [Text] -> Sh ()
clean args = cabal ("new-clean" : args)

configure :: [Text] -> Sh ()
configure args = cabal ("new-configure" : args)

buildFontTest :: Sh ()
buildFontTest = build ["--project", "server.project", "happstack-ghcjs-server:fonttest"]

buildScope :: Sh ()
buildScope = build ["--project", "server.project", "appraisalscope"]


chdir' :: Shelly.FilePath -> Sh a -> Sh a
chdir' dir action = liftIO (putStrLn (" cd> " <> show dir)) >> chdir dir action


successHook :: Sh ()
successHook =
  do asyncSh $
       do b <- liftIO $ doesFileExist "success-hook.sh"
          when b $ run_ "./success-hook.sh" []
     pure ()

uhohHook :: Sh ()
uhohHook =
  do asyncSh $
       do b <- liftIO $ doesFileExist "uhoh-hook.sh"
          when b $ run_ "./uhoh-hook.sh" []
     pure ()

