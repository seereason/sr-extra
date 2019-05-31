-- | Various redundant versions of shell command running code
-- collected here from several different places.

{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wredundant-constraints #-}
module Extra.Process
    ( CreateProcess
    , timeTask
    , RunOptions(..)
    -- * builders for RunOptions
    , showCommand
    , showCommandAndResult
    , putIndented
    -- * Simple process runners
    , run
    , runV, runVE
    , runQ, runQE
    , runIO
    , testExit
    , processException
    , insertProcessEnv
    , modifyProcessEnv
    -- * Refugees
    , runV2, runVE2, runQ2, runQE2
    , run2
    , runV3, runQE3
    ) where

import Control.Arrow (second)
import Control.Exception (evaluate, Exception, IOException, throw)
import Control.Lens (view)
import Control.Monad.Catch (catch, MonadCatch, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (evalState, StateT, get, put)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ListLike (break, head, hPutStr, null, singleton, tail)
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(fromString))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (diffUTCTime, getCurrentTime, NominalDiffTime)
import Data.Typeable (typeOf)
import Extra.Except (liftIOError, MonadIOError, withError, withException, HasLoc(withLoc))
import Extra.EnvPath (HasEnvRoot(envRootLens), rootPath)
import Extra.Verbosity (ePutStrLn)
import Extra.TH (here, prettyLocs)
import GHC.IO.Exception (IOErrorType(OtherError))
import Language.Haskell.TH.Syntax (Loc)
import Prelude hiding (break, head, null, tail)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stdout, stderr)
import System.IO.Error (mkIOError)
import System.Process (CreateProcess(cwd, env))
import System.Process.ListLike (Chunk(..), collectOutput, ListLikeProcessIO, readCreateProcessLazy, readCreateProcessWithExitCode, showCreateProcessForUser)

-- | Run a task and return the elapsed time along with its result.
timeTask :: MonadIO m => m a -> m (a, NominalDiffTime)
timeTask x =
    do start <- liftIO $ getCurrentTime
       result <- x >>= liftIO . evaluate
       finish <- liftIO $ getCurrentTime
       return (result, diffUTCTime finish start)

data RunOptions a m
    = StartMessage (String -> CreateProcess -> m ())
    | OverOutput ([Chunk a] -> m [Chunk a]) -- e.g. putIndented
    | FinishMessage (String -> CreateProcess -> (ExitCode, a, a) -> m ())
    -- | Verbosity Int
    | RunOptions [RunOptions a m]

instance Semigroup (RunOptions a m) where
    RunOptions a <> RunOptions b = RunOptions (a <> b)
    RunOptions a <> b = RunOptions (a <> [b])
    a <> RunOptions b = RunOptions ([a] <> b)
    a <> b = RunOptions [a, b]

showCommand :: MonadIO m => String -> CreateProcess -> m ()
showCommand prefix p = ePutStrLn (prefix ++ showCreateProcessForUser p)

showCommandAndResult :: MonadIO m => [Char] -> CreateProcess -> (ExitCode, a, a) -> m ()
showCommandAndResult prefix p (code, _, _) =
    ePutStrLn (prefix ++ showCreateProcessForUser p ++ " -> " ++ show code)

putIndented :: forall a c m. (Eq c, ListLikeProcessIO a c, IsString a, MonadIO m) => [Chunk a] -> m [Chunk a]
putIndented chunks =
    liftIO $ mapM_ echo (indentChunks "     1> " "     2> " chunks) >> return chunks
    where
      echo :: Chunk a -> IO (Chunk a)
      echo c@(Stdout x) = hPutStr stdout x >> return c
      echo c@(Stderr x) = hPutStr stderr x >> return c
      echo c = return c

run ::
    forall a c e m. (HasLoc e, MonadIOError e m, ListLikeProcessIO a c)
    => RunOptions a m
    -> CreateProcess
    -> a
    -> m (ExitCode, a, a)
run opts p input = do
  start " -> " p
  (result :: (ExitCode, a, a)) <- withError (withLoc $here) $ liftIOError (readCreateProcessLazy p input) >>= overOutput >>= return . collectOutput
  finish " <- " p result
  return result
    where
      -- We need the options as a Foldable type
      opts' :: [RunOptions a m]
      opts' = case opts of RunOptions xs -> xs; x -> [x]
      start :: String -> CreateProcess -> m ()
      start = foldr (\o f -> case o of (StartMessage f') -> f'; _ -> f) (\_ _ -> pure ()) opts'
      finish :: String -> CreateProcess -> (ExitCode, a, a) -> m ()
      finish = foldr (\o f -> case o of (FinishMessage f') -> f'; _ -> f) (\_ _ _ -> pure ()) opts'
      overOutput :: [Chunk a] -> m [Chunk a]
      overOutput = foldr (\o f -> case o of (OverOutput f') -> f'; _ -> f) return opts'

--runVE :: (Eq c, IsString a, ListLikeProcessIO a c, MonadIO m, MonadCatch m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
--runVE p input = try $ runV p input

runV ::
    (Eq c, IsString a, ListLikeProcessIO a c, HasLoc e, MonadIOError e m)
    => CreateProcess -> a -> m (ExitCode, a, a)
runV p input = run (StartMessage showCommand <> OverOutput putIndented <> FinishMessage showCommandAndResult) p input

runVE ::
    (Eq c, IsString a, ListLikeProcessIO a c, MonadCatch m, HasLoc e, MonadIOError e m, Exception e)
    => CreateProcess -> a -> m (Either e (ExitCode, a, a))
runVE p i = try $ runV p i

--runQE :: (ListLikeProcessIO a c, MonadIO m, MonadCatch m) => CreateProcess -> a -> m (Either SomeException (ExitCode, a, a))
--runQE p input = try $ runQ p input

runQ ::
    (ListLikeProcessIO a c, HasLoc e, MonadIOError e m)
    => CreateProcess -> a -> m (ExitCode, a, a)
runQ p input = run (StartMessage showCommand <> FinishMessage showCommandAndResult) p input

runQE ::
    (ListLikeProcessIO a c, MonadCatch m, HasLoc e, MonadIOError e m, Exception e)
    => CreateProcess -> a -> m (Either e (ExitCode, a, a))
runQE p i = try $ runQ p i

runIO :: CreateProcess -> IO L.ByteString
runIO cp = do
  (code, out, err) <- readCreateProcessWithExitCode cp L.empty
  case code of
    ExitSuccess -> return out
    ExitFailure _ -> throwError $ userError $ unlines $
                                       [ show code
                                       , " command: " ++ showCreateProcessForUser cp
                                       , " stderr: " ++ unpack (decodeUtf8 (L.toStrict err))
                                       , " stdout: " ++ unpack (decodeUtf8 (L.toStrict out)) ]

-- | Pure function to indent the text of a chunk list.
indentChunks :: forall a c. (ListLikeProcessIO a c, Eq c, IsString a) => String -> String -> [Chunk a] -> [Chunk a]
indentChunks outp errp chunks =
    evalState (Prelude.concat <$> mapM (indentChunk nl (fromString outp) (fromString errp)) chunks) BOL
    where
      nl :: c
      nl = Data.ListLike.head (fromString "\n" :: a)

-- | The monad state, are we at the beginning of a line or the middle?
data BOL = BOL | MOL deriving (Eq)

-- | Indent the text of a chunk with the prefixes given for stdout and
-- stderr.  The state monad keeps track of whether we are at the
-- beginning of a line - when we are and more text comes we insert one
-- of the prefixes.
indentChunk :: forall a c m. (Monad m, ListLikeProcessIO a c, Eq c) => c -> a -> a -> Chunk a -> StateT BOL m [Chunk a]
indentChunk nl outp errp chunk =
    case chunk of
      Stdout x -> doText Stdout outp x
      Stderr x -> doText Stderr errp x
      _ -> return [chunk]
    where
      doText :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doText con pre x = do
        let (hd, tl) = break (== nl) x
        (<>) <$> doHead con pre hd <*> doTail con pre tl
      doHead :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doHead _ _ x | null x = return []
      doHead con pre x = do
        bol <- get
        case bol of
          BOL -> put MOL >> return [con (pre <> x)]
          MOL -> return [con x]
      doTail :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doTail _ _ x | null x = return []
      doTail con pre x = do
        bol <- get
        put BOL
        tl <- doText con pre (tail x)
        return $ (if bol == BOL then [con pre] else []) <> [con (singleton nl)] <> tl

-- | Turn process exit codes into exceptions.
{-
throwProcessResult' :: (ExitCode -> Maybe IOError) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult' f p chunks = mapResultM (\ code -> maybe (return $ Result code) (throw $ processException p code) (f code)) chunks

-- | Turn process exit codes into exceptions with access to the
-- original CreateProcess record.
throwProcessResult'' :: Exception e => (CreateProcess -> ExitCode -> Maybe e) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult'' f p chunks = mapResultM (\ code -> maybe (return $ Result code) throw (f p code)) chunks

throwProcessFailure :: CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessFailure p = throwProcessResult' (testExit Nothing (Just . processException p . ExitFailure)) p

mapResultM :: Monad m => (ExitCode -> m (Chunk a)) -> [Chunk a] -> m [Chunk a]
mapResultM f chunks = mapM (\ x -> case x of Result code -> f code; _ -> return x) chunks
-}

testExit :: a -> (Int -> a) -> ExitCode -> a
testExit s _ ExitSuccess = s
testExit _ f (ExitFailure n) = f n

-- | Copied from "System.Process", the exception thrown when the
-- process started by 'System.Process.readProcess' gets an
-- 'ExitFailure'.
processException :: CreateProcess -> ExitCode -> IOError
processException p code =
    mkIOError OtherError (showCreateProcessForUser p ++ maybe "" (\ d -> "(in " ++ show d ++ ")") (cwd p) ++ " -> " ++ show code) Nothing Nothing

-- | Set an environment variable in the CreateProcess, initializing it
-- with what is in the current environment.
insertProcessEnv :: [(String, String)] -> CreateProcess -> IO CreateProcess
insertProcessEnv pairs = modifyProcessEnv (map (second Just) pairs)

modEnv1 :: [(String, String)] -> (String, Maybe String) -> [(String, String)]
modEnv1 env0 (name, mvalue) = maybe [] (\ v -> [(name, v)]) mvalue ++ filter ((/= name) . fst) env0

modifyProcessEnv :: MonadIO m => [(String, Maybe String)] -> CreateProcess -> m CreateProcess
modifyProcessEnv pairs p = do
  env0 <- liftIO $ maybe getEnvironment return (env p)
  let env' = foldl modEnv1 env0 pairs
  return $ p {env = Just env'}

runV2 ::
    (MonadIO m, MonadCatch m, Eq c, IsString a, ListLikeProcessIO a c, MonadError e m)
    => [Loc] -> CreateProcess -> a -> m (ExitCode, a, a)
runV2 locs p input =
    run2 locs (StartMessage (showCommand' locs) <> OverOutput putIndented <> FinishMessage showCommandAndResult) p input

runVE2 ::
    forall a c e m. (Eq c, IsString a, ListLikeProcessIO a c, MonadIO m, MonadCatch m, MonadError e m, Exception e)
    => [Loc] -> CreateProcess -> a -> m (Either e (ExitCode, a, a))
runVE2 locs p input = do
    try (runV2 locs p input)

runQ2 ::
    (MonadIO m, MonadCatch m, ListLikeProcessIO a c, MonadError e m)
    => [Loc] -> CreateProcess -> a -> m (ExitCode, a, a)
runQ2 locs p input =
    run2 locs (StartMessage (showCommand' locs) <> FinishMessage showCommandAndResult) p input

runQE2 ::
    (ListLikeProcessIO a c, MonadIO m, MonadCatch m, MonadError e m, Exception e)
    => [Loc] -> CreateProcess -> a -> m (Either e (ExitCode, a, a))
runQE2 locs p input =
    try (runQ2 locs p input)

showCommand' :: (MonadIO m{-, HasOSKey r, MonadReader r m-}) => [Loc] -> String -> CreateProcess -> m ()
showCommand' locs prefix p = do
  -- key <- view osKey
  ePutStrLn (prefix ++ showCreateProcessForUser p ++ " (" ++ {-"in " ++ show key ++ ", " ++-} "called from " ++ show (prettyLocs ($here : locs)) ++ ")")

run2 ::
    forall a c e m. (MonadError e m, ListLikeProcessIO a c, MonadIO m, MonadCatch m)
    => [Loc]
    -> RunOptions a m
    -> CreateProcess
    -> a
    -> m (ExitCode, a, a)
run2 locs opts p input = do
  start " -> " p
  (result :: (ExitCode, a, a)) <- catch (liftIO (readCreateProcessLazy p input) >>= overOutput >>= return . collectOutput)
                                        (\se -> withException (\e -> liftIO (hPutStrLn stderr ("(at " ++ show (prettyLocs ($here : locs)) ++ ": " ++ show e ++ ") :: " ++ show (typeOf e)))) se >> throw se)
  finish " <- " p result
  liftIO $ evaluate result
    where
      -- We need the options as a Foldable type
      opts' :: [RunOptions a m]
      opts' = case opts of RunOptions xs -> xs; x -> [x]
      start :: String -> CreateProcess -> m ()
      start = foldr (\o f -> case o of (StartMessage f') -> f'; _ -> f) (\_ _ -> pure ()) opts'
      finish :: String -> CreateProcess -> (ExitCode, a, a) -> m ()
      finish = foldr (\o f -> case o of (FinishMessage f') -> f'; _ -> f) (\_ _ _ -> pure ()) opts'
      overOutput :: [Chunk a] -> m [Chunk a]
      overOutput = foldr (\o f -> case o of (OverOutput f') -> f'; _ -> f) return opts'

-- stray copies of the stuff above that I moved here, with MonadApt constraints

runV3 ::
    (Eq c, IsString a, ListLikeProcessIO a c, HasEnvRoot r, MonadReader r m, MonadError e m, MonadIO m, MonadCatch m)
    => [Loc] -> CreateProcess -> a -> m (ExitCode, a, a)
runV3 locs p input =
    run2 locs (StartMessage showCommand3' <> OverOutput putIndented <> FinishMessage showCommandAndResult) p input

{-
runVE3 ::
    (Eq c, IsString a, ListLikeProcessIO a c, MonadApt r m, MonadError e m, MonadIO m, MonadCatch m)
    => [Loc] -> CreateProcess -> a -> m (Either IOException (ExitCode, a, a))
runVE3 locs p input = try $ runV3 locs p input
-}

runQ3 ::
    (ListLikeProcessIO a c, HasEnvRoot r, MonadReader r m, MonadError e m, MonadIO m, MonadCatch m)
    => [Loc] -> CreateProcess -> a -> m (ExitCode, a, a)
runQ3 locs p input =
    run2 locs (StartMessage showCommand3' <> FinishMessage showCommandAndResult) p input

runQE3 ::
    (ListLikeProcessIO a c, HasEnvRoot r, MonadReader r m, MonadError e m, MonadIO m, MonadCatch m)
    => [Loc] -> CreateProcess -> a -> m (Either IOException (ExitCode, a, a))
runQE3 locs p input = try $ runQ3 locs p input

showCommand3' :: (HasEnvRoot r, MonadReader r m, MonadIO m) => String -> CreateProcess -> m ()
showCommand3' prefix p = do
  key <- view (envRootLens . rootPath)
  ePutStrLn (prefix ++ showCreateProcessForUser p ++ " (in " ++ show key ++ ")")
