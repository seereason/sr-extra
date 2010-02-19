{-# LANGUAGE ForeignFunctionInterface #-}
-- Copyright Stefan O'Rear 2006
-- Copyright Jeremy Shaw 2007
module Extra.Terminal where

import System.Posix.Env
import Foreign.C.Types

foreign import ccall "gwinsz.h c_get_window_size" c_get_window_size :: IO CLong

-- get the number of rows and columns using ioctl (0, TIOCGWINSZ, &w)
-- @see also: getWidth
getWinSize :: IO (Int,Int)
getWinSize = do (a,b) <- (`divMod` 65536) `fmap` c_get_window_size
                return (fromIntegral b, fromIntegral a)

-- get the number of colums.
-- First tries getWinSize, if that returns 0, then try the COLUMNS
-- shell variable.
getWidth :: IO (Maybe Int)
getWidth =
    do (cols, _) <- getWinSize
       case cols of
         0 -> return . fmap read =<< getEnv "COLUMNS"
         _ -> return (Just cols)
