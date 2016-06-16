module Extra.HaXml where

import Text.PrettyPrint
import Text.XML.HaXml
import Text.XML.HaXml.Pretty

-- ** XML Helper functions (move)
-- | Render XML as a string.
-- MOVE: ??
showXML :: String -> CFilter -> Doc
showXML styleSheet = document . mkDocument styleSheet . cfilterToElem

-- MOVE: ??
mkTxt :: String -> CFilter
mkTxt = cdata

-- MOVE: ??
-- cliff says this is broken with regards to cdata
cfilterToElem :: CFilter -> Element
cfilterToElem f = case f (CString False "") of
                    [CElem e] -> xmlEscape stdXmlEscaper e
                    []        -> error "RSS produced no output"
                    _         -> error "RSS produced more than one output"

-- <?xml-stylesheet type="text/xsl" href="cdcatalog.xsl"?>
-- MOVE: ??
mkDocument :: String -> Element -> Document
mkDocument styleSheet elem =
    let xmlDecl = XMLDecl "1.0" (Just (EncodingDecl "utf-8")) (Just True)
        prolog   = Prolog (Just xmlDecl)  [] Nothing [PI ("xml-stylesheet","type=\"text/xsl\" href=\""++styleSheet++"\"")]
        symTable = []
    in
      Document prolog [] elem []
