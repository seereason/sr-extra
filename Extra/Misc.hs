module Extra.Misc
    (-- * List functions
    -- * String functions
      columns
    , justify
    -- * Tuple functions
    , mapSnd
    -- * FilePath functions
    , parentPath
    , canon
    -- * Map and Set functions
    , listMap
    , listDiff
    -- * Either functions
    -- * System.IO functions
    --ePut,
    --ePutList,
    -- * System.Posix
    , checkSuperUser
    -- removeRecursiveSafely,
    , md5sum
    , sameInode
    , sameMd5sum
    , tarDir
    -- * Processes
    , Extra.Misc.processOutput
    , processOutput2
    , splitOutput
    -- ByteString
    , cd
    -- Debugging
    , read'
    ) where

import		 Control.Exception
import		 Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import qualified Data.Set as Set
import		 Extra.List
import		 System.FilePath
import		 System.Unix.Process
import		 System.Directory
import		 System.Exit
import		 System.IO
import		 System.Posix.Files
import		 System.Posix.User (getEffectiveUserID)
import		 Text.Regex

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- Control file stuff

{-
mergeControls :: [Control] -> Control
mergeControls (Control p1 : Control p2 : etc) = mergeControls (Control (p1 ++ p2) : etc)
mergeControls [Control p1]  = Control p1
mergeControls [] = Control []    

fieldValue :: String -> Paragraph -> Maybe String
fieldValue fieldName paragraph =
    maybe Nothing value (lookupP fieldName paragraph)
    where value (Field (_, value)) = (Just . stripWS) value

hasField :: String -> String -> Paragraph -> Bool
hasField field value paragraph =
    maybe False (== value) (fieldValue field paragraph)
-}

-- |Pad strings so the columns line up. The argument and return value
-- elements are the rows of a table.  Do not pad the rightmost column.
columns :: [[String]] -> [[String]]
columns rows =
    map (map pad . zip (widths ++ [0])) rows
    where
      widths = map (fromJust . listMax) . transpose . map (map length . init) $ rows
      listMax l = foldl (\ a b -> Just . maybe b (max b) $ a) Nothing l
      pad (width, field) = field ++ replicate (max 0 (width - length field)) ' '

-- |Group words into lines of length n or less.
justify :: String -> Int -> [[String]]
justify s n =
    foldr doWord [[]] (words s)
    where doWord w [] = [[w]]
	  doWord w (ws : etc) = 
	      if length (concat (intersperse " " (w:ws))) <= n then
		 (w : ws) : etc else
		 [w] : ws : etc

-- |dirname
parentPath :: FilePath -> FilePath
parentPath path = fst (splitFileName path)

{-
baseName :: FilePath -> String
baseName path = snd (splitFileName path)
-}

-- |Turn a list of (k, a) pairs into a map from k -> [a].  The order of the elements in
-- the a list is preserved.
listMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
listMap pairs =
    foldl insertPair Map.empty (reverse pairs)
    where insertPair m (k,a) = Map.insert k (a : (Map.findWithDefault [] k m)) m

-- Return the difference of two lists.  Order is not preserved.
listDiff :: Ord a => [a] -> [a] -> [a]
listDiff a b = Set.toList (Set.difference (Set.fromList a) (Set.fromList b))

-- | Weak attempt at canonicalizing a file path.
canon :: FilePath -> FilePath
canon path =
    let re = mkRegex "/" in
    let names = splitRegex re path in
    concat (intersperse "/" (merge names))
    where
      merge (".." : xs) = ".." : (merge xs)
      merge ("." : xs) = "." : (merge xs)
      merge (_ : ".." : xs) = (merge xs)
      merge (x : "." : xs) = (merge (x : xs))
      merge (x : xs) = x : merge xs
      merge [] = []

-- | Run md5sum on a file and return the resulting checksum as text.
md5sum :: FilePath -> IO (Either String String)
md5sum path =
    do output <- lazyCommand cmd B.empty
       let result =
               case exitCodeOnly output of
                 [ExitFailure n] -> Left ("Error " ++ show n ++ " running '" ++ cmd ++ "'")
                 [ExitSuccess] ->
                     case listToMaybe . words . B.unpack . stdoutOnly $ output of
                       Nothing -> Left $ "Error in output of '" ++ cmd ++ "'"
                       Just checksum -> Right checksum
                 _ -> Left "Internal error 12"
       return result
    where
      cmd = "md5sum " ++ path

-- | Predicate to decide if two files have the same inode.
sameInode :: FilePath -> FilePath -> IO Bool
sameInode a b =
    do
      aStatus <- getFileStatus a
      bStatus <- getFileStatus b
      return (deviceID aStatus == deviceID bStatus && fileID aStatus == fileID bStatus)

-- | Predicate to decide if two files have the same md5 checksum.
sameMd5sum :: FilePath -> FilePath -> IO Bool
sameMd5sum a b =
    do
      asum <- md5sum a
      bsum <- md5sum b
      return (asum == bsum)

-- | Backwards compatibility functions.
processOutput :: String -> IO (Either Int String)
processOutput command =
    do
      output <- lazyCommand command B.empty
      case exitCodeOnly output of
        [ExitSuccess] -> return . Right . B.unpack . stdoutOnly $ output
        [ExitFailure n] -> return . Left $ n
        _ -> error "My.processOutput: Internal error 13"

processOutput2 :: String -> IO (String, ExitCode)
processOutput2 command =
    do
      output <- lazyCommand command B.empty
      case exitCodeOnly output of
        [code] -> return ((B.unpack . stdoutOnly $ output), code)
        _ -> error "My.processOutput2: Internal error 14"

splitOutput :: [Output] -> (B.ByteString, B.ByteString, Maybe ExitCode)
splitOutput output = (stdoutOnly output, stderrOnly output, listToMaybe (exitCodeOnly output))

-- |A version of read with a more helpful error message.
read' s =
    case reads s of
      [] -> error $ "read - no parse: " ++ show s
      ((x, s) : _) -> x

{-
type DryRunFn = IO () -> (Bool, String) -> IO ()

(-?-) :: DryRunFn
-- ^ If this is a dry run (dryRun params is True) do *not* evaluate f,
-- but instead print a message.
(-?-) f (dryRun, "") = if dryRun then return () else f
(-?-) f (dryRun, msg) =
    do
      hPutStrLn stderr msg
      if dryRun then return () else f
infixr 9 -?-
-}

{-
(+/+) :: FilePath -> FilePath -> FilePath
(+/+) path1 "" = path1
(+/+) "" path2 = path2
(+/+) path1 path2 =
    case (last path1, head path2) of
      ('/', '/') -> path1 +/+ (tail path2)
      (_, '/') -> path1 ++ path2
      ('/', _) -> path1 ++ path2
      (_, _) -> path1 ++ "/" ++ path2
-}

{-
-------------- From MissingH --------------

-- | Split the path into directory and file name
--
-- Examples:
--
-- \[Posix\]
--
-- > splitFileName "/"            == ("/",    ".")
-- > splitFileName "/foo/bar.ext" == ("/foo", "bar.ext")
-- > splitFileName "bar.ext"      == (".",    "bar.ext")
-- > splitFileName "/foo/."       == ("/foo", ".")
-- > splitFileName "/foo/.."      == ("/foo", "..")
--
-- \[Windows\]
--
-- > splitFileName "\\"               == ("\\",      "")
-- > splitFileName "c:\\foo\\bar.ext" == ("c:\\foo", "bar.ext")
-- > splitFileName "bar.ext"          == (".",       "bar.ext")
-- > splitFileName "c:\\foo\\."       == ("c:\\foo", ".")
-- > splitFileName "c:\\foo\\.."      == ("c:\\foo", "..")
--
-- The first case in the Windows examples returns an empty file name.
-- This is a special case because the \"\\\\\" path doesn\'t refer to
-- an object (file or directory) which resides within a directory.

splitFileName :: FilePath -> (String, String)
splitFileName p = (reverse path1, reverse fname1)
  where
    (fname,path) = break isPathSeparator (reverse p)
    path1 = case path of
      "" -> "."
      _  -> case dropWhile isPathSeparator path of
	"" -> [pathSeparator]
	p  -> p
    fname1 = case fname of
      "" -> "."
      _  -> fname

-- | Checks whether the character is a valid path separator for the host
-- platform. The valid character is a 'pathSeparator' but since the Windows
-- operating system also accepts a slash (\"\/\") since DOS 2, the function
-- checks for it on this platform, too.
isPathSeparator :: Char -> Bool
isPathSeparator ch =
  ch == '/'

-- | Provides a platform-specific character used to separate directory levels in
-- a path string that reflects a hierarchical file system organization. The
-- separator is a slash (@\"\/\"@) on Unix and Macintosh, and a backslash
-- (@\"\\\"@) on the Windows operating system.
pathSeparator :: Char
pathSeparator = '/'
-}


checkSuperUser :: IO Bool
checkSuperUser = getEffectiveUserID >>= return . (== 0)

-- | Given a tarball, return the name of the top directory.
tarDir :: FilePath -> IO (Maybe String)
tarDir path =
    Extra.Misc.processOutput cmd >>=
      return . either (const Nothing) (dir . lines)
    where
      cmd = "tar tfz '" ++ path ++ "'"
      dir [] = Nothing
      dir (file : _) = case wordsBy (== '/') file of
                         [] -> Nothing
                         ("" : _) -> Nothing
                         (s : _) -> Just s

cd :: FilePath -> IO a -> IO a
cd name m = 
    bracket 
        (do cwd <- getCurrentDirectory
            setCurrentDirectory name
            return cwd)
        (\oldwd -> do setCurrentDirectory oldwd)
        (const m)
