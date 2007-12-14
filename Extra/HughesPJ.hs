module Extra.HughesPJ where

import Data.Maybe
import Extra.Terminal
import Text.PrettyPrint.HughesPJ

-- |render a Doc using the current terminal width
renderWidth :: Doc -> IO String       
renderWidth doc =
    do columns <- return . fromMaybe 80 =<< getWidth
       return $ renderStyle (Style PageMode columns 1.0) doc
