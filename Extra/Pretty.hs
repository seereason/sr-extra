-- | A constructor we can wrap around values to avoid any built in
-- Pretty instance - for example, instance Pretty [a].
--
--  * display is now prettyShow
--  * display' is now prettyText
--  * ppDisplay is now ppShow
--  * ppDisplay' is now ppText

{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}

module Extra.Pretty
    ( PP(PP, unPP)
    , prettyText
    , ppPrint
    , pprPair
    , pprSet
    , ppShow
    , ppText
    , pprint1, pprint1'
    , pprintW
    , friendlyNames
    -- * Re-export
    , prettyShow
    , renderW
    ) where

import Data.Data (Data)
import Data.Generics (everywhere, mkT)
import Data.Set as Set (Set, toList)
import Data.Text (Text, unpack, pack)
import Distribution.Pretty (Pretty(pretty), prettyShow)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib as TH (Doc, hcat, hsep, ptext, to_HPJ_Doc)
import Language.Haskell.TH.Syntax
import qualified Text.PrettyPrint as HPJ
import Text.PrettyPrint.HughesPJClass as HPJ (Doc, text, empty)

-- | This type is wrapped around values before we pretty print them so
-- we can write our own Pretty instances for common types without
-- polluting the name space of clients of this package with instances
-- they don't want.
newtype PP a = PP {unPP :: a} deriving (Functor)

instance Pretty (PP Text) where
    pretty = text . unpack . unPP

instance Pretty (PP String) where
    pretty = text . unPP

instance Pretty (PP a) => Pretty (PP (Maybe a)) where
    pretty = maybe empty ppPrint . unPP

prettyText :: Pretty a => a -> Text
prettyText = pack . prettyShow

ppPrint :: Pretty (PP a) => a -> HPJ.Doc
ppPrint = pretty . PP

ppShow :: Pretty (PP a) => a -> String
ppShow = prettyShow . PP

ppText :: Pretty (PP a) => a -> Text
ppText = pack . prettyShow . PP

-- | Make a template haskell value more human reader friendly.  The
-- result may or may not be compilable.  That's ok, though, because
-- the input is usually uncompilable - it imports hidden modules, uses
-- infix operators in invalid positions, puts module qualifiers in
-- places where they are not allowed, and maybe other things.
friendlyNames :: Data a => a -> a
friendlyNames =
    everywhere (mkT friendlyName)
    where
      friendlyName (Name x _) = Name x NameS -- Remove all module qualifiers

-- | Render a 'Doc' on a single line.
render1 :: TH.Doc -> String
render1 = HPJ.renderStyle (HPJ.style {HPJ.mode = HPJ.OneLineMode}) . to_HPJ_Doc

-- | Render a 'Doc' with the given width.
renderW :: Int -> TH.Doc -> String
renderW w = HPJ.renderStyle (HPJ.style {HPJ.lineLength = w}) . to_HPJ_Doc

-- | Pretty print the result of 'render1'.
pprint1' :: Ppr a => a -> [Char]
pprint1' = render1 . ppr

-- | 'pprint1'' with friendlyNames.
pprint1 :: (Ppr a, Data a) => a -> [Char]
pprint1 = pprint1' . friendlyNames

-- | Pretty print the result of 'renderW'.
pprintW' :: Ppr a => Int -> a -> [Char]
pprintW' w = renderW w . ppr

-- | 'pprintW'' with friendly names.
pprintW :: (Ppr a, Data a) => Int -> a -> [Char]
pprintW w = pprintW' w . friendlyNames

pprPair :: (Ppr a, Ppr b) => (a, b) -> TH.Doc
pprPair (a, b) = hcat [ptext "(", ppr a, ptext ", ", ppr b, ptext ")"]

pprSet :: Ppr a => Set a -> TH.Doc
pprSet s = hcat [ptext "[", hsep (fmap ppr (Set.toList s)), ptext "]"]
