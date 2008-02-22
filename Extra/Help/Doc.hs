module Extra.Help.Doc
    ( manToDoc
    , elementsToDoc
    , reduce
    , eToI
    ) where

import qualified Text.PrettyPrint.HughesPJ as D

import Extra.Help.Man
import Extra.Help.Markup

manToDoc :: Man -> D.Doc
manToDoc (Man _mTitle body) = elementsToDoc body

elementsToDoc :: Elements -> D.Doc
elementsToDoc (Elements elements) = iToDoc $ eToI (reduce elements)

reduce :: [Element] -> [Element]
reduce [] = []
reduce (Font _ : es) = reduce es
reduce (Text' t : es) =
    case reduce es of
      (Text' u : ess) -> Text' (textMerge (reduceText t) u) : ess
      (Break : ess) -> Text' (reduceText t) : ess
      es' -> Text' (reduceText t) : es'
    where
      textMerge (Text t) (Text u) = Text [Str (reduceElms (t ++ [Str " "] ++ u))]
reduce ((Section heading) : es) = Section (reduceText heading) : reduce es
reduce (e : es) = e : reduce es

-- eToI :: [Element] -> [Intermediate]
eToI [] = []
eToI ((Text' (Text [Str s])) : es) =
    case eToI es of
      ((IText strings) : rest) -> (IText (s : strings) : rest)
      rest -> (IText [s] : rest)
eToI (RS n : es) = -- this is wacky, perhaps eToI needs a state variable with the current nesting depth
    let (children, rest) = findEnd 0 [] es
    in
      (INest n (eToI children)) : (eToI rest)
    where
      notChild (Paragraph _) = False
      notChild (Section _) = True
      notChild (SubSection _) = True
      notChild (Font _) = False
      notChild (RS _) = False
      notChild (RE _) = False
      notChild Break = False
      notChild (Text' _) = False
      notChild Empty = False
      findEnd n children (e@(RS _) : rest) =
          findEnd (n + 1) (e : children) rest
      findEnd n children (e@(RE _m) : rest) -- FIXME: undo specified number of levels
          | n == 0 = ((reverse (e : children)), rest)
          | otherwise = findEnd (n - 1) (e:children) rest
      findEnd n children es@(e : rest)
          | notChild e = (reverse children, es)
          | otherwise = findEnd n (e : children) rest
      findEnd _n children [] =
          (reverse children, [])
eToI (RE _ : es) = eToI es
eToI (Break : es) = eToI es
eToI ((Section heading) : es) =
    let (children, rest) = break notChild es
    in
      (ISection (textToString' heading) (eToI children)) : (eToI rest)
eToI ((SubSection heading) : es) =
    let (children, rest) = break notChild es
    in
      (ISubSection (textToString' heading) (eToI children)) : (eToI rest)
eToI ((Paragraph ps) : es) = 
    let (children, rest) = break notChild es
    in
      (IParagraph ps (eToI children)) : (eToI rest)
    where
      notChild (Paragraph _) = True
      notChild (Section _) = True
      notChild (SubSection _) = True
      notChild (Font _) = False
      notChild (RS _) = False
      notChild (RE _) = False
      notChild Break = False
      notChild (Text' _) = False
      notChild Empty = False
eToI e = error ("eToI: unhandled element: " ++ show e)


notChild (Paragraph _) = False
notChild (Section _) = True
notChild (SubSection _) = True
notChild (Font _) = False
notChild (RS _) = False
notChild (RE _) = False
notChild Break = False
notChild (Text' _) = False
notChild Empty = False

data Intermediate
    = IText [String]
    | INest (Maybe NNN) [Intermediate]
    | IParagraph ParagraphStyle [Intermediate]
    | ISection String [Intermediate]
    | ISubSection String [Intermediate]
      deriving (Eq, Show, Read)

iToDoc :: [Intermediate] -> D.Doc
iToDoc = foldr (D.$+$) D.empty . map ppIntermediate 

ppIntermediate :: Intermediate -> D.Doc
ppIntermediate (IText strs) = foldr (D.$+$) D.empty (map (D.fsep . map D.text . words) strs)
ppIntermediate (INest Nothing i) = D.nest 8 (iToDoc i)
ppIntermediate (IParagraph ps i) = ppParagraph ps i
ppIntermediate (ISection title children) = D.text "" D.$+$ D.text title D.$+$ D.nest 8 (iToDoc children)
ppIntermediate (ISubSection title children) = D.nest 4 $ D.text title D.$+$ D.nest 4 (iToDoc children)

ppParagraph PlainParagraph i = (iToDoc i D.$+$ (D.text ""))
ppParagraph (TP indent label) i = D.text (textToString (reduceText label)) D.$$ (D.nest (nnnToSpaces indent) (iToDoc i)) D.$+$ D.text ""

nnnToSpaces :: Maybe NNN -> Int
nnnToSpaces Nothing = 7
nnnToSpaces (Just (DefaultUnit n)) = n
