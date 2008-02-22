module Extra.Help.Markup where

import Data.Monoid

newtype Elements = Elements [Element]
      deriving (Show, Read, Eq)

instance Monoid Elements where
    mempty = Elements []
    (Elements xs) `mappend` (Elements ys) = Elements (xs ++ ys)

data Element
    = Paragraph ParagraphStyle -- label font is not changed, but body is
    | Section Text -- this resets the margin/font
    | SubSection Text -- this resets the margin/font
    | Font Font
    | RS (Maybe NNN)
    | RE (Maybe NNN)
    | Break
    | Text' Text
    | Empty
      deriving (Show, Read, Eq)

data Font = R | B | I | CW | P
      deriving (Show, Read, Eq, Enum)

newtype Text
    = Text [TxtElm]
      deriving (Show, Read, Eq)

data TxtElm 
    = Str String
    | Escape' Escape
      deriving (Show, Read, Eq)

data Escape
    = EFont Font
      deriving (Show, Read, Eq)

data ParagraphStyle 
    = PlainParagraph
    | IndentedParagraph (Maybe String) (Maybe NNN)
    | HangingParagraph (Maybe NNN)
    | TP (Maybe NNN) Text
      deriving (Show, Read, Eq)

data NNN 
    = DefaultUnit Int
      deriving (Show, Read, Eq)


textToString' :: Text -> String
textToString' (Text [Str s]) = s

textToString :: Text -> String
textToString t = textToString' (reduceText t)


reduceText :: Text -> Text
reduceText (Text elms) = Text [Str (reduceElms elms)]
 
reduceElms [] = []
reduceElms (Str s : es) = s ++ reduceElms es
reduceElms (Escape' (EFont _) : es) = {-" " ++ -} reduceElms es
