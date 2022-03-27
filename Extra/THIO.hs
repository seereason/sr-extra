-- | Write template haskell splices to a file in such a way that it
-- could then be #included by ghcjs.  Compile error if the file
-- already exists and is different from the generated text.

{-# LANGUAGE NoOverloadedStrings, RecordWildCards #-}

module Extra.THIO
    ( testAndWriteSplices
    , writeSplicesWith
    -- , fixStrings
    , writeSplices
    , pprintW'
    , pprintStyle
    , safeName
    , spliceModule
    , spliceModuleNew
    ) where

import Data.Generics (Data, everywhere, mkT)
-- import Data.List (stripPrefix)
-- import Data.List.Extra (dropSuffix)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (pack, stripSuffix, Text)
import Extra.IO (testAndWriteDotNew, writeFileWithBackup)
import Language.Haskell.TH (Dec, Loc(..), location, Ppr, ppr, Q, runIO)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.PprLib (Doc, to_HPJ_Doc)
import Language.Haskell.TH.Syntax (addDependentFile, Name(..), OccName(..), NameFlavour(..))
import System.Directory (doesDirectoryExist)
import System.FilePath (splitFileName, takeDirectory)
import qualified Text.PrettyPrint as HPJ
import Text.Regex.TDFA ((=~), MatchResult(MR))

spliceModule :: Text -> [Dec] -> Q [Dec]
spliceModule header decs = do
  splicesPath <- (\Loc{..} -> let (dir, file) = splitFileName loc_filename in dir <> "SplicesFor" <> file) <$> location
  testAndWriteSplicesWithHeader header splicesPath decs

spliceModuleNew :: Text -> [Dec] -> Q [Dec]
spliceModuleNew header decs = do
  splicesPath <- (\Loc{..} -> let (dir, file) = splitFileName loc_filename in dir <> "SplicesFor" <> file) <$> location
  addDependentFile splicesPath
  testAndWriteSplicesWithHeader header splicesPath decs

#if 0
dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix a b = fromMaybe b $ stripSuffix a b

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b = reverse <$> stripPrefix (reverse a) (reverse b)
#endif

testAndWriteSplicesWithHeader ::
  (Data a, Ppr a)
  => Text
  -> FilePath
  -> a
  -> Q a
testAndWriteSplicesWithHeader header =
  writeSplicesWith (\path text -> testAndWriteDotNew path (ensureFinalNewline (header <> text))) ppr

ensureFinalNewline :: Text -> Text
ensureFinalNewline t = fromMaybe t (stripSuffix (pack "\n") t) <> pack "\n"

-- | Write some template haskell splices to a file so they can be
-- re-read by ghcjs.
testAndWriteSplices ::
    (Data a, Ppr a)
    => FilePath -- ^ Module name for the generated file
    -> a -- ^ decs generated by 'Data.Path.TH.makeUnion' and 'Data.Path.TH.makePaths'
    -> Q a
testAndWriteSplices = writeSplicesWith testAndWriteDotNew ppr

-- | Pretty print with a specified line length.
pprintW' :: (Ppr a, Data a) => Int -> a -> [Char]
pprintW' w = pprintStyle (HPJ.style {HPJ.lineLength = w})

-- | Pretty print with a specified 'HPJ.Style'
pprintStyle :: (Ppr a, Data a) => HPJ.Style -> a -> String
pprintStyle style = HPJ.renderStyle style . to_HPJ_Doc . ppr . everywhere (mkT safeName)

-- | Names with the best chance of compiling when prettyprinted:
--    * Remove all package and module names
--    * Remove suffixes on all constructor names
--    * Remove suffixes on the four ids we export
--    * Leave suffixes on all variables and type variables
safeName :: Name -> Name
safeName (Name oc (NameG _ns _pn _mn)) = Name oc NameS
safeName (Name oc (NameQ _mn)) = Name oc NameS
safeName (Name oc@(OccName _) (NameU _)) = Name oc NameS
safeName name@(Name _ (NameL _)) = name -- Not seeing any of these
safeName name@(Name _ NameS) = name

-- | Turn this:
-- @@
--   (Name (mkOccName "AppraisalValue")
--     (NameG TcClsName (mkPkgName "appra_B8Hqp4MOZTzG2RIYHT6sPz")
--        (mkModName "Appraisal.ReportTH")))
-- @@
-- into this:
-- @@
--   $(lift 'Appraisal.ReportTH.AppraisalValue).
-- @@
-- This is applied to all declarations in the generated splice file.
fixNames :: String -> String
fixNames s =
    let s' = fixStrings s in
    case (s' =~ "\\(Name \\(mkOccName \\\"([^\"]*)\"\\) \\((NameG ([^ ]*) \\(mkPkgName \\\"([^\"]*)\\\"\\) \\(mkModName \\\"([^\"]*)\\\"\\)|NameU [0-9]*)\\)\\)" :: MatchResult String) of
      MR before _ after [name, _, "", "", ""] _ -> before <> "(mkName " <> show name <> ") " <> fixNames after
      MR before _ after [name, _, "VarName", _, _modpath] _ -> before <>  "'" <> name <> fixNames after
      MR before _ after [name, _, "DataName", _, _modpath] _ -> before <>  "''" <> name <> fixNames after
      MR before _ after [name, _, "TcClsName", _, _modpath] _ -> before <>  "''" <> name <> fixNames after -- I think this is right
      MR before _ _ _ _ -> before
#if 0
    where
      -- Convert a NameSpace constructor to a name quote
      typeOrValue "DataName" =  "lookupValueName"
      typeOrValue "TcClsName" = "lookupTypeName"
      typeOrValue "VarName" = "lookupValueName"
      typeOrValue _ = error "Unexpected NameSpace constructor"
#endif

ws :: String
ws = "(\t|\r|\n| |,)*"
ch :: String
ch = "'([^'\\\\]|\\\\')'"

fixStrings :: String -> String
fixStrings s =
    let MR before _ after xs _ = s =~ ("\\[((" <> ws <> ch <> ")*" <> ws <> ")\\]") :: MatchResult String in
    case xs of
      [] -> s -- eof
      ["", _, _, _, _] -> before <> "[]" <> fixStrings after -- empty list
      [cs, _, _, _, _] -> before <> "\"" <> fixChars cs <> "\"" <> fixStrings after
      _ -> error "Regular expression failure"

fixChars :: String -> String
fixChars "" = ""
fixChars s =
    let MR before _ after [_, c, _] _ = s =~ (ws <> ch <> ws) :: MatchResult String in before <> fixChar c <> fixChars after
    where
      fixChar "\\'" = "'"
      fixChar s' = s'

-- | Write the splices unconditionally (compare 'testAndWriteSplices'.)
writeSplices :: (Data a, Ppr a) => FilePath -> [a] -> Q [a]
writeSplices = writeSplicesWith writeFileWithBackup ppr

#if 0
-- | Newlines in text strings are badly munged by the pretty printer
fixText :: Data a => a -> a
fixText = everywhere (mkT (fixText1 :: Lit -> Lit))
    where
      fixText1 (StringL s) = StringL (filter (/= '\n') s)
      fixText1 x = x
#endif

-- | Write the paths using a supplied writer.
writeSplicesWith :: Data a => (FilePath -> Text -> IO ()) -> (a -> Doc) -> String -> a -> Q a
writeSplicesWith writer pretty dest decs = do
  exists <- runIO (doesDirectoryExist (takeDirectory dest))
  case exists of
    False -> error $ "Could not write splices file to " ++ show dest
    True -> do
      let new = pack $ fixNames
                     $ HPJ.renderStyle (HPJ.style {HPJ.lineLength = 1000000 {-HPJ.mode = HPJ.OneLineMode-}})
                     $ to_HPJ_Doc
                     $ pretty
                     $ everywhere (mkT safeName)
                     $ {-fixText-} decs
      runIO $ writer dest new
      return decs
