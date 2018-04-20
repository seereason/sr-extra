{-# LANGUAGE CPP #-}

module Extra.Net where

#if 0
import qualified Data.ByteString.Lazy.Char8 as L
import		 Data.Maybe
import		 Text.Regex

-- | Parse the text returned when a directory is listed by a web
-- server.  This is currently only known to work with Apache.
-- NOTE: there is a second copy of this function in
-- debian:Debian.URI. Please update both locations if you make
-- changes.
webServerDirectoryContents :: L.ByteString -> [String]
webServerDirectoryContents text =
    catMaybes . map (second . matchRegex re) . lines . L.unpack $ text
    where
      re = mkRegex "( <A HREF|<a href)=\"([^/][^\"]*)/\""
      second (Just [_, b]) = Just b
      second _ = Nothing
#endif
