{-# LANGUAGE TypeSynonymInstances #-}
-- |A value of type TIO represents the state of the terminal I/O
-- system.  The 'bol' flag keeps track of whether we are at the
-- beginning of line on the console.  This is computed in terms of
-- what we have sent to the console, but it should be remembered that
-- the order that stdout and stderr are sent to the console may not be
-- the same as the order in which they show up there.  However, in
-- practice this seems to work as one would hope.
module Extra.TIO
    ( module Extra.CIO
    -- * The TIO monad
    , TIO
    , runTIO
    , tryTIO
    --, liftTIO
    ) where

import Extra.CIO
import Prelude hiding (putStr, putChar, putStrLn)
import Control.Exception
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Trans
import qualified System.IO as IO

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

-- |Perform a TIO monad task in the IO monad.
runTIO :: TStyle -> TIO a -> IO a
runTIO style action = (runRWST action) style initState >>= \ (a, _, _) -> return a

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

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: TState
initState = TState {cursor = BOL}

-- |The TIO instance of CIO adds some features to the normal console
-- output.  By tracking the cursor position it is able to insert a
-- prefix to each line, and to implement a "beginning of line" (BOL)
-- function which only adds a newline when the cursor is not already
-- at BOL.  It also allows verbosity controlled output, where a verbosity
-- level is stored in the monad state and output requests are given a
-- verbosity which must be greater or equal to the monad's verbosity
-- level for the output to appear.
instance CIO TIO where
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
          prefix style = liftIO (IO.hPutStr IO.stderr (hGetPrefix h style)) -- >> io (IO.hFlush h)
          newline = liftIO (IO.hPutStr IO.stderr "\n") -- >> io (IO.hFlush IO.stderr)
          write s = liftIO (IO.hPutStr IO.stderr s) -- >> io (IO.hFlush h)
    -- | A "virtual" newline, this puts us into the EOL state.  From
    -- this state, a newline will be inserted before the next output,
    -- unless that output itself begins with a newline.
    hBOL h =
        do state <- get
           put (state {cursor = if cursor state == BOL then BOL else EOL})
    -- |Return the "effective verbosity", or perhaps the effective
    -- quietness.  If this value is zero or less the output will be
    -- complete.  By convention, if it is one, the output will be brief.
    -- If it is a two or more no output is generated.
    ev v =
        do style <- ask
           return (verbosity style - v)
    -- |Modify the current style for this action
    setStyle styleFn = local styleFn
    -- |Implementation of try for the TIO monad
    tryCIO = tryTIO

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
