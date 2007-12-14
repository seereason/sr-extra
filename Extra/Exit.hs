module Extra.Exit where

import Extra.HughesPJ
import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint.HughesPJ

-- |exitFailure with nicely formatted help text on stderr
exitWithHelp :: (String -> Doc) -- ^ generate help text, the argument is the result of getProgName
             -> IO a -- ^ no value is returned, this function always calls exitFailure
exitWithHelp helpText =
    do progName <- getProgName
       hPutStrLn stderr =<< renderWidth (helpText progName)
       exitFailure
