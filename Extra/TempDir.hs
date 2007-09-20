module Extra.TempDir 
    ( mkdtemp
    ) where

import Foreign.C.String

foreign import ccall "mkdtemp" c_mkdtemp :: CString -> IO CString


-- this is buggy, use the version in Linspire.Unix.Directory instead
mkdtemp :: String -> IO String
mkdtemp template =
    do
      s <- newCString template
      d <- c_mkdtemp s
      peekCString d
