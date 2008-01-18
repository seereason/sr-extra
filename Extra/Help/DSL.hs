module Extra.Help.DSL
    ( (<>)
    , text, te
    , b, i, r, cw, p, font
    , pp, lp, ip, hp, tp
    , br
    , section, subsection
    , title
    , indent, rs
    , punctuate
    ) where

import Data.List
import Extra.Help.Man
import System.Console.GetOpt

class MkText a where
    text :: String -> a

instance MkText Text where
    text = Text . (:[]) . Str

instance MkText Elements where
    text = Elements . (:[]) . Text' . Text . (:[]) . Str

-- |lift Text to Elements
te :: Text -> Elements
te t = Elements [Text' t]

class Append a where
    append :: a -> a -> a
    
instance Append Text where
    (Text s) `append` (Text t) = Text (s ++ t)

instance Append [a] where
    x `append` y = x ++ y

instance Append Elements where
    (Elements []) `append` ys = ys
    (Elements [Text' t]) `append` (Elements (Text' u : ys)) =
        Elements (Text' (t `append` u) : ys)
    (Elements xs) `append` (Elements ys) = Elements (xs ++ ys)

(<>) :: Append a => a -> a -> a
(<>) = append
    
class MkFont a where
    mkfont :: Font -> a

instance MkFont Text where
    mkfont f = Text [Escape' (EFont f)]

instance MkFont Elements where
    mkfont f = Elements [Text' (Text [Escape' (EFont f)])]

b :: MkFont a => a
b = mkfont B

i :: MkFont a => a
i = mkfont I

r :: MkFont a => a
r = mkfont R

cw :: MkFont a => a
cw = mkfont CW

p :: MkFont a => a
p = mkfont P

font :: Font -> Elements
font f = Elements [Font f]

pp :: Elements
pp = Elements [Paragraph PlainParagraph]

lp :: Elements
lp = Elements [Paragraph PlainParagraph]

ip :: Maybe String -> Maybe NNN -> Elements
ip designator indent = Elements [Paragraph (IndentedParagraph designator indent)]

hp :: Maybe NNN -> Elements
hp indent = Elements [Paragraph (HangingParagraph indent)]

tp :: Maybe NNN -> Text -> Elements
tp indent label = Elements [Paragraph (TP indent label)]

br :: Elements
br = Elements [Break]

section :: Text -> Elements
section = Elements . (:[]) . Section

subsection :: Text -> Elements
subsection = Elements . (:[]) . SubSection

rs :: Maybe NNN -> Elements -> Elements
rs n (Elements children) = Elements (RS n : children ++ [RE Nothing])

indent :: Maybe NNN -> Elements -> Elements
indent = rs 

title :: String -> Section -> Maybe Title
title titleStr section = Just (Title titleStr section Nothing)

punctuate :: Text -> [Text] -> Text
punctuate _ [] = Text []
punctuate p txts = flattenText (intersperse p txts)
    where
      flattenText [] = Text []
      flattenText txts = Text (concatMap unText txts)
      unText (Text elms) = elms

