{-# LANGUAGE CPP, RankNTypes #-}

module Extra.Text
    ( Texty(textyString, textyText, textyLazy)
    , diffText
    , camelWords
    , capitalize
    , Describe(describe')
    , describe
    , textshow
    , trunc
#if !__GHCJS__
    , tests
#endif
    ) where

import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.Char (isUpper, toUpper)
import Data.ListLike (groupBy)
import Data.String (IsString)
import Data.Text (split, Text, pack, unpack)
import qualified Data.Text.Lazy as Lazy ( fromStrict, pack, Text, toStrict, unpack )
#if !__GHCJS__
import Test.HUnit (assertEqual, Test(TestCase, TestList))
#endif
import qualified Text.PrettyPrint as HPJ
-- * Texty

class (IsString a, Monoid a) => Texty a where
  textyString :: String -> a
  textyText :: Text -> a
  textyLazy :: Lazy.Text -> a

instance Texty Text where
  textyString = pack
  textyText = id
  textyLazy = Lazy.toStrict

instance Texty String where
  textyString = id
  textyText = unpack
  textyLazy = Lazy.unpack

instance Texty Lazy.Text where
  textyString = Lazy.pack
  textyText = Lazy.fromStrict
  textyLazy = id

-- | The ever needed, never available show that returns a Text.
textshow :: (Texty text, Show a) => a -> text
textshow = textyString . show

-- | Output the difference between two string in the style of diff(1).  This
-- can be used with Test.HUnit.assertString:  assertString (diffText ("a", "1\n2\n3\n"), ("b", "1\n3\n"))
diffText :: (String, Text) -> (String, Text) -> String
diffText (nameA, textA) (nameB, textB) =
    show (prettyContextDiff
          (HPJ.text nameA)
          (HPJ.text nameB)
          (HPJ.text . unpack)
          (getContextDiff 2 (split (== '\n') textA) (split (== '\n') textB)))

-- | Convert a camel case string (no whitespace) into a natural
-- language looking phrase:
--   camelWords "aCamelCaseFOObar123" -> "A Camel Case FOObar123"
camelWords :: String -> String
camelWords s =
    case groupBy (\ a b -> isUpper a == isUpper b) (dropWhile (== '_') s) of -- "aCamelCaseFOObar123"
      (x : xs) -> concat $ capitalize x : map (\ (c : cs) -> if isUpper c then ' ' : c : cs else c : cs) xs
      [] -> ""

#if !__GHCJS__
-- Most of these fail.
tests :: Test
tests =
    TestList
    [ TestCase (assertEqual "camel words 0" "A Camel Case FOO Bar 123" (camelWords "aCamelCaseFOOBar123"))
    , TestCase (assertEqual "camel words 1" "My Generator" (camelWords "myGenerator"))
    , TestCase (assertEqual "camel words 2" "PDF Generator" (camelWords "pDFGenerator"))
    , TestCase (assertEqual "camel words 3" "PDF Generator" (camelWords "PDFGenerator"))
    , TestCase (assertEqual "camel words 4" "Report PDF Generator" (camelWords "reportPDFGenerator")) ]
#endif

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c) : cs

-- | Override the default description associated with the type of @a@.
-- The first argument indicates the field of the parent record that
-- contains the @a@ value, if any.
class Describe a where
    describe' :: Maybe String -> a -> Maybe String

describe :: Describe a => a -> Maybe String
describe = describe' Nothing

-- | Truncate a string to avoid writing monster lines into the log.
trunc :: String -> String
trunc s = if length s > 1000 then take 1000 s ++ "..." else s
