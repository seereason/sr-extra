module Extra.Net
    ( webServerDirectoryContents
    ) where

import qualified Data.ByteString.Char8 as B
import		 Data.Maybe
import		 Text.Regex

-- | Parse the text returned when a directory is listed by a web
-- server.  This is currently only known to work with Apache.
webServerDirectoryContents :: B.ByteString -> [String]
webServerDirectoryContents text =
    catMaybes . map (second . matchRegex re) . lines . B.unpack $ text
    where
      re = mkRegex "( <A HREF|<a href)=\"([^/][^\"]*)/\""
      second (Just [_, b]) = Just b
      second _ = Nothing
