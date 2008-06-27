-- |CIO is a type class for the TIO monad, which tracks the cursor
-- position of the console so that indentation and prefixes can be
-- added to the output.  TIO also has a style component which lets you
-- control the output verbosity and the appearance of the prefix.
-- There is an instance for the regular IO monad which doesn't use any
-- of these features, to allow functions which do not use the TIO
-- monad call functions in the Debian library.
module Extra.CIO 
    ( -- * The CIO class
      CIO(..)

    -- * Style constructors and transformers
    , TStyle(..)
    , defStyle
    , withStyle

    , setVerbosity
    , addVerbosity
    , setPrefix
    , addPrefix
    , appPrefix
    , setPrefixes
    , addPrefixes
    , appPrefixes
    , hGetPrefix

    -- * Output functions
    , putStr
    , ePutStr
    , vPutStr
    , vEPutStr

    , hPutChar
    , putChar
    , ePutChar
    , vHPutChar
    , vPutChar
    , vEPutChar

    , hPutStrBl
    , putStrBl
    , ePutStrBl
    , vHPutStrBl
    , vPutStrBl
    , vEPutStrBl

    , hPutStrLn
    , putStrLn
    , ePutStrLn
    , vHPutStrLn
    , vPutStrLn
    , vEPutStrLn

    , bol
    , eBOL
    , vHBOL
    , vBOL
    , vEBOL
    ) where

import Prelude hiding (putStr, putChar, putStrLn)
import qualified System.IO as IO
import Control.Monad.Trans

-- |Class representing ways of doing console (terminal?) output.
class MonadIO m => CIO m where
    -- |Write output to a handle.
    hPutStr :: IO.Handle -> String -> m ()
    -- |If we are not already at the beginning of a line, move the cursor
    -- to the beginning of the next one.
    hBOL :: IO.Handle -> m ()
    -- |Return the \"effective verbosity\" for a task.  If the argument
    -- is 2 it means the caller is computing ev for a task that
    -- normally does output when the verbosity level is 2 or higher.
    -- If the verbosity of the current style is 1, then the ev or
    -- effective verbosity is 2-1 = -1, so the output should be
    -- quieter.
    ev :: Int -> m Int
    -- |Modify the current output style.
    setStyle :: (TStyle -> TStyle) -> m a -> m a

-- |A record used to hold the output style information for a task.
-- This The prefixes that will appear at the beginning of each line,
-- and the desired verbosity level.  Suggested verbosity level policy:
--
--  * -1: No output of any kind, as if you were directing all output to /dev/null
-- 
--  * 0: Error output only, suitable for a run whose log you might examine later
-- 
--  * 1: casual progress reporting - if you were running on a console but didn't
--      expect anything to go wrong
--
--  * 2: detailed progress reporting - show more progress, particularly things
--      that might fail during the normal operation of the autobuilder: patches
--      that fail to apply, dpkg-buildpackage runs that return errors, etc.
--
--  * 3: Debugging output - use this level or higher if you suspect the
--      autobuilder itself is failing, or you are doing development work on
--      the autobuilder.
data TStyle
    = TStyle { prefix :: String			-- ^ Add this string at the beginning of each line
             , verbosity :: Int			-- ^ Ignore v functions whose argument is more than this
             , hPrefix :: [(IO.Handle, String)]	-- ^ Per-handle prefix
             } deriving Show

-- |Make IO an instance of CIO.  The instance ignores all style and
-- state information.  This allows you to call all the Debian
-- functions from the standard IO monad.
instance CIO IO where
    hPutStr h s = IO.hPutStr h s
    hBOL h = IO.hPutStr h "\n"
    ev v = return v
    setStyle _ f = f

defStyle :: TStyle
defStyle = TStyle { prefix = ": "
                  , hPrefix = []
                  , verbosity = 0
                  }

-- |Use a new style for the TIO action
withStyle :: CIO m => TStyle -> m a -> m a
withStyle newStyle = setStyle (const newStyle)

setVerbosity :: Int -> TStyle -> TStyle
setVerbosity n style = style {verbosity = n}
addVerbosity :: Int -> TStyle -> TStyle
addVerbosity n style = setVerbosity (n + verbosity style) style

-- |Set the output style for a handle to prefixed.
setPrefix :: String -> TStyle -> TStyle
setPrefix prefix style = style {prefix = prefix}

-- | Prepend some text to the prefix.
addPrefix :: String -> TStyle -> TStyle
addPrefix newPrefix style = style {prefix = newPrefix ++ prefix style}

-- | Append some text to the prefix.
appPrefix :: String -> TStyle -> TStyle
appPrefix newPrefix style = style {prefix = prefix style ++ newPrefix}

hSetPrefix :: IO.Handle -> String -> TStyle -> TStyle
hSetPrefix handle string style =
    style {hPrefix = (handle, string) : filter ((/= handle) . fst) (hPrefix style)}

hAddPrefix :: IO.Handle -> String -> TStyle -> TStyle
hAddPrefix handle string style =
    hSetPrefix handle (string ++ prefix) style
    where prefix = maybe "" id (lookup handle (hPrefix style))

hAppPrefix :: IO.Handle -> String -> TStyle -> TStyle
hAppPrefix handle string style =
    hSetPrefix handle (prefix ++ string) style
    where prefix = maybe "" id (lookup handle (hPrefix style))

-- |Get the current prefix for a particular handle
hGetPrefix :: IO.Handle -> TStyle -> String
hGetPrefix handle style = prefix style ++ maybe "" id (lookup handle (hPrefix style))

-- |Set the output style for the stdout and stderr handle to prefixed,
-- using whatever prefixes were most recently set (default is [1] and [2].)
setPrefixes :: String -> String -> TStyle -> TStyle
setPrefixes stdoutPrefix stderrPrefix style =
    hSetPrefix IO.stdout stdoutPrefix . hSetPrefix IO.stderr stderrPrefix $ style

-- |Switch to prefixed mode and modify both the stdout and stderr prefixes.
addPrefixes :: String -> String -> TStyle -> TStyle
addPrefixes oPrefix ePrefix style =
    hAddPrefix IO.stdout oPrefix . hAddPrefix IO.stderr ePrefix $ style

appPrefixes :: String -> String -> TStyle -> TStyle
appPrefixes oPrefix ePrefix style =
    hAppPrefix IO.stdout oPrefix . hAppPrefix IO.stderr ePrefix $ style

-- |Perform an action if the effective verbosity level is >= 0,
-- otherwise return the default value d.
vIO :: CIO m => Int -> a -> m a -> m a
vIO v d f =
    do v' <- ev v
       if v' >= 0 then f else return d

-- |Write a string to stdout.
putStr :: CIO m => String -> m ()
putStr = hPutStr IO.stdout

-- |Write a string to stderr.
ePutStr :: CIO m => String -> m ()
ePutStr = hPutStr IO.stderr

-- |Verbosity controlled version of ePutStr
vEPutStr :: CIO m => Int -> String -> m ()
vEPutStr = vHPutStr IO.stderr

-- |Write a string to stdout depending on the verbosity level.
vPutStr :: CIO m => Int -> String -> m ()
vPutStr = vHPutStr IO.stdout

-- |Write a character.
hPutChar :: CIO m => IO.Handle -> Char -> m ()
hPutChar h c = hPutStr h [c]

-- |Write a character to stdout.
putChar :: CIO m => Char -> m ()
putChar = hPutChar IO.stdout

-- |Write a character to stderr.
ePutChar :: CIO m => Char -> m ()
ePutChar = hPutChar IO.stderr

-- |Verbosity controlled version of hPutStr
vHPutStr :: CIO m => IO.Handle -> Int -> String -> m ()
vHPutStr h v s = vIO v () (hPutStr h s)

-- |Verbosity controlled version of hPutChar.
vHPutChar :: CIO m => IO.Handle -> Int -> Char -> m ()
vHPutChar h v c = vHPutStr h v [c]

-- |Verbosity controlled version of putChar
vPutChar :: CIO m => Int -> Char -> m ()
vPutChar = vHPutChar IO.stdout

-- |Verbosity controlled version of ePutChar
vEPutChar :: CIO m => Int -> Char -> m ()
vEPutChar = vHPutChar IO.stderr

-- |Move to beginning of next line (if necessary) and output a string.
hPutStrBl :: CIO m => IO.Handle -> String -> m ()
hPutStrBl h s = hBOL h >> hPutStr h s

-- |hPutStrBl to stdout.
putStrBl :: CIO m => String -> m ()
putStrBl = hPutStrBl IO.stdout

-- |hPutStrBl to stderr.
ePutStrBl :: CIO m => String -> m ()
ePutStrBl = hPutStrBl IO.stderr

-- |Verbosity controlled version of hPutStrBl
vHPutStrBl :: CIO m => IO.Handle -> Int -> String -> m ()
vHPutStrBl h v s = vHBOL h v >> vHPutStr h v s

-- |Verbosity controlled version of putStrBl
vPutStrBl :: CIO m => Int -> String -> m ()
vPutStrBl = vHPutStrBl IO.stdout          

-- |Verbosity controlled version of ePutStrBl
vEPutStrBl :: CIO m => Int -> String -> m ()
vEPutStrBl = vHPutStrBl IO.stderr

-- |Write a newline character and a string.
hPutStrLn :: CIO m => IO.Handle -> String -> m ()
hPutStrLn h s = hBOL h >> hPutStr h s

-- |hPutStrLn to stdout.
putStrLn :: CIO m => String -> m ()
putStrLn s = hPutStrLn IO.stdout s

-- |hPutStrLn to stderr.
ePutStrLn :: CIO m => String -> m ()
ePutStrLn = hPutStrLn IO.stderr

-- |Verbosity controlled version of hPutStrLn.
vHPutStrLn :: CIO m => IO.Handle -> Int -> String -> m ()
vHPutStrLn h v s = vHBOL h v >> vHPutStr h v s

-- |Verbosity controlled version of putStrLn
vPutStrLn :: CIO m => Int -> String -> m ()
vPutStrLn = vHPutStrLn IO.stdout          

-- |Verbosity controlled version of ePutStrLn
vEPutStrLn :: CIO m => Int -> String -> m ()
vEPutStrLn = vHPutStrLn IO.stderr

-- |hBOL to stdout.
bol :: CIO m => m ()
bol = hBOL IO.stdout

-- |hBOL to stderr.
eBOL :: CIO m => m ()
eBOL = hBOL IO.stderr

vHBOL :: CIO m => IO.Handle -> Int -> m ()
vHBOL h v = vIO v () (hBOL h)

-- |Verbosity controlled version of BOL
vBOL :: CIO m => Int -> m ()
vBOL = vHBOL IO.stdout

-- |Verbosity controlled version of eBOL
vEBOL :: CIO m => Int -> m ()
vEBOL = vHBOL IO.stderr

-- These only work in a terminal, not in an emacs shell.
hColor h s = case h of
               _ | h == IO.stdout -> green s
               _ | h == IO.stdout -> red s
               _ -> magenta s

blue s = "\ESC[34m" ++ s ++ "\ESC[30m"
green s = "\ESC[32m" ++ s ++ "\ESC[30m"
red s = "\ESC[31m" ++ s ++ "\ESC[30m"
magenta s = "\ESC[35m" ++ s ++ "\ESC[30m"
