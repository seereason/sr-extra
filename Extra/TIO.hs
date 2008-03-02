module Debian.TIO
    ( TIO
    , TStyle(..)
    , runTIO
    , tryTIO
    , liftTIO
    -- * Style constructors and transformers
    , defStyle
    , setStyle
    , withStyle
    , setPrefix
    , addPrefix
    , appPrefix
    , setPrefixes
    , addPrefixes
    , appPrefixes
    , setVerbosity
    , addVerbosity

    -- * Output to stdout
    , putStr
    , putChar
    , putStrBl
    , bol

    -- * Output to stderr
    , ePutStr
    , ePutChar
    , ePutStrBl
    , eBOL

    -- * Verbosity controlled output
    , vPutStr
    , vPutChar
    , vPutStrBl
    , vBOL
    , vEPutStr
    , vEPutChar
    , vEPutStrBl
    , vEBOL
    , ev
    ) where

import Prelude hiding (putStr, putChar, putStrLn)
import Control.Exception
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Trans
import qualified System.IO as IO

-- |A style used to process the output generated by a task.  This
-- includes messages produced at the beginning and end of the task,
-- how the output of the task is formatted, and style information
-- assigned to individual output handles.
--
-- Suggested verbosity level policy:
-- <0 - No output of any kind, if you were directing all output to /dev/null
--  0 - Error output only, suitable for a run whose log you might examine later
--  1 - casual progress reporting - if you were running on a console but didn't
--      expect anything to go wrong
--  2 - detailed progress reporting - show more progress, particularly things
--      that might fail during the normal operation of the autobuilder: patches
--      that fail to apply, dpkg-buildpackage runs that return errors, etc.
--  3 - Debugging output - use this level or higher if you suspect the
--      autobuilder itself is failing, or you are doing development work on
--      the autobuilder.
data TStyle
    = TStyle { prefix :: String			-- ^ Add this string at the beginning of each line
             , verbosity :: Int			-- ^ Ignore v functions whose argument is more than this
             , hPrefix :: [(IO.Handle, String)]	-- ^ Per-handle prefix
             } deriving Show

-- | This represents the state of the IO system.  The 'bol' flag keeps
-- track of whether we are at the beginning of line on the console.
-- This is computed in terms of what we have sent to the console, but
-- it should be remembered that the order that stdout and stderr are
-- sent to the console may not be the same as the order in which they
-- show up there.  However, in practice this seems to work as one
-- would hope.
data TState
    = TState { cursor :: Position		-- ^ Is the console at beginning of line?
             } deriving Show

data Position
    = BOL	-- Beginning of line
    | MOL	-- Middle of line
    | EOL	-- End of line
    deriving (Show, Eq)

type TIOT = RWST TStyle () TState
type TIO = TIOT IO

-- |mark an action that should be run in the regular IO monad
io :: IO a -> TIO a
io = liftIO

-- |Perform a TIO monad task in the IO monad.
runTIO :: TStyle -> TIO a -> IO a
runTIO style action = (runRWST action) style initState >>= \ (a, _, _) -> return a

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: TState
initState = TState {cursor = BOL}

defStyle :: TStyle
defStyle = TStyle { prefix = ": "
                  , hPrefix = []
                  , verbosity = 0
                  }

-- |Modify the current style for this TIO action
setStyle :: (TStyle -> TStyle) -> TIO a -> TIO a
setStyle styleFn = local styleFn

-- |Use a new style for the TIO action
withStyle :: TStyle -> TIO a -> TIO a
withStyle newStyle = setStyle (const newStyle)

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

-- Output a string, maintaining the state and using the current style.
hPutStr :: IO.Handle -> String -> TIO ()
hPutStr h s =
    do style <- ask
       state <- get
       case (cursor state, break (== '\n') s) of
         (_, ("", "")) -> return ()
         (BOL, ("", (_ : b))) -> prefix style >> newline >> hPutStr h b
         (MOL, ("", (_ : b))) -> newline >> put (state {cursor = BOL}) >> hPutStr h b
         (EOL, ("", (_ : b))) -> newline >> put (state {cursor = BOL}) >> hPutStr h b
         (BOL, (a, b)) -> prefix style >> write a >> put (state {cursor = MOL}) >> hPutStr h b
         (MOL, (a, b)) -> write a >> hPutStr h b
         (EOL, (a, b)) -> newline >> prefix style >> write a >> put (state {cursor = MOL}) >> hPutStr h b
    where
      prefix style = io (IO.hPutStr IO.stderr (hGetPrefix h style)) -- >> io (IO.hFlush h)
      newline = io (IO.hPutStr IO.stderr "\n") -- >> io (IO.hFlush IO.stderr)
      write s = io (IO.hPutStr IO.stderr s) -- >> io (IO.hFlush h)

-- These only work in a terminal, not in an emacs shell.
hColor h s = case h of
               _ | h == IO.stdout -> green s
               _ | h == IO.stdout -> red s
               _ -> magenta s

blue s = "\ESC[34m" ++ s ++ "\ESC[30m"
green s = "\ESC[32m" ++ s ++ "\ESC[30m"
red s = "\ESC[31m" ++ s ++ "\ESC[30m"
magenta s = "\ESC[35m" ++ s ++ "\ESC[30m"

hPutChar h c = hPutStr h [c]
--hPutStrLn h s = hPutStr h (s ++ "\n")
hPutStrLn h s = hBOL h >> hPutStr h s
hPutStrBl h s = hBOL h >> hPutStr h s

-- A "virtual" newline, this puts us into the EOL state.  A
-- newline will be inserted before the next output, unless that
-- output itself begins with a newline.
hBOL :: IO.Handle -> TIO ()
hBOL _ =
    do state <- get
       put (state {cursor = if cursor state == BOL then BOL else EOL})

putStr = hPutStr IO.stdout
--putStrLn = hPutStrLn IO.stdout
putStrLn = hPutStrLn IO.stdout
putStrBl = hPutStrBl IO.stdout
putChar = hPutChar IO.stdout
bol = hBOL IO.stdout

ePutStr = hPutStr IO.stderr
--ePutStrLn = hPutStrLn IO.stderr
ePutStrLn = hPutStrLn IO.stderr
ePutStrBl = hPutStrBl IO.stderr
ePutChar = hPutChar IO.stderr
eBOL = hBOL IO.stderr

setVerbosity :: Int -> TStyle -> TStyle
setVerbosity n style = style {verbosity = n}
addVerbosity :: Int -> TStyle -> TStyle
addVerbosity n style = setVerbosity (n + verbosity style) style

-- |Output function with a verbosity setting.  There is a current verbosity level
-- in the TIO monad, and more output is generated when it is higher.  When we pass
-- a higher verbosity level to the output command here, it makes the output less
-- verbose, so that calling vHPutStr with a verbosity level of 2 means that there
-- will only be normal output if the verbosity level in the monad is raised to 2.
vHPutStr :: IO.Handle -> Int -> String -> TIO ()
vHPutStr h v s =
    do style <- ask
       if verbosity style >= v then hPutStr h ({- "(v=" ++ show (verbosity style) ++ "-" ++ show v ++ ")" ++ -} s) else return ()

vHPutChar h v c = vHPutStr h v [c]
--vHPutStrLn h v s = vHPutStr h v (s ++ "\n")
vHPutStrLn h v s = vHBOL h v >> vHPutStr h v s
vHPutStrBl h v s = vHBOL h v >> vHPutStr h v s

vHBOL :: IO.Handle -> Int -> TIO ()
vHBOL h v =
    do style <- ask
       if verbosity style >= v then hBOL h else return ()

-- |Return the "effective verbosity", or perhaps the effective
-- quietness.  If this value is zero or less the output will be
-- complete.  By convention, if it is one, the output will be brief.
-- If it is a two or more no output is generated.
ev :: Int -> TIO Int
ev v =
    do style <- ask
       return (verbosity style - v)

vPutStr = vHPutStr IO.stdout
--vPutStrLn = vHPutStrLn IO.stdout
vPutStrLn = vHPutStrLn IO.stdout          
vPutStrBl = vHPutStrBl IO.stdout          
vPutChar = vHPutChar IO.stdout
vBOL = vHBOL IO.stdout

vEPutStr = vHPutStr IO.stderr
--vEPutStrLn = vHPutStrLn IO.stderr
vEPutStrLn = vHPutStrLn IO.stderr
vEPutStrBl = vHPutStrBl IO.stderr
vEPutChar = vHPutChar IO.stderr
vEBOL = vHBOL IO.stderr

_test :: IO ()
_test =
    runTIO (defStyle {prefix = "% "})
               (putStr "hello\nworld\n" >>
                bol >>					-- No extra newline
                putStr "(some text)" >>
                putStr "(some more on same line)" >>	-- This should be right after abc
                bol >>
                putStr "(newline)\n\n(after a blank line)" >>	-- This should show up on a new line
                bol)					-- Newline before exit

-- |Catch exceptions in a TIO action.
tryTIO :: TIO a -> TIO (Either Exception a)
tryTIO task =
    do state <- get
       liftTIO (try' state) task
    where
      try' state task =
          do result <- try task
             case result of
               Left e -> return (Left e, state, ())
               Right (a, s, _) -> return (Right a, s, ())

liftTIO :: (IO (a, TState, ()) -> IO (b, TState, ())) -> TIO a -> TIO b
liftTIO f = mapRWST f