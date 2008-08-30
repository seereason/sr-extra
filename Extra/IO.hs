module Extra.IO where

import Extra.CIO
import qualified System.IO as IO
import Control.Exception (try)

-- |Use this module to call functions in the CIO module from the
-- regular IO monad.  This instance ignores all style and state
-- information.  The verbosity controlled output functions will ignore
-- any calls when v is greater than zero.  This allows you to call the
-- functions in the haskell-debian package from the regular IO monad.
-- 
-- This is in a separate module from CIO so you don't accidentally do
-- a liftIO of some other CIO operation and get this instance.
instance CIO IO where
    hPutStr h s = IO.hPutStr h s
    hBOL h = IO.hPutStr h "\n"
    ev v = return (- v)
    setStyle _ f = f
    tryCIO = try
