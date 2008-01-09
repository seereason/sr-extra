module Extra.Help.Groff 
    ( ppMan 
    )
    where

import Extra.Help.Man
import Text.PrettyPrint.HughesPJ hiding (Str)

ppMan :: Man -> Doc
ppMan (Man mTitle elements) =
    (maybe empty ppTitle mTitle) $$ ppElements elements $+$ text ""

ppElements :: Elements -> Doc
ppElements (Elements elements) = vcat (map ppElement elements)

ppTitle :: Title -> Doc
ppTitle (Title name section extra) =
    text ".TH" <+> ppStr True name <+> ppSection section <+> ppExtra extra
    where
      ppExtra Nothing = empty
      ppExtra (Just (dateStr, extra2)) = ppStr True dateStr <+> ppExtra2 extra2
      ppExtra2 Nothing = empty
      ppExtra2 (Just (source, extra3)) = ppStr True source <+> ppExtra3 extra3
      ppExtra3 Nothing = empty
      ppExtra3 (Just manual) = ppStr True manual

ppSection :: Section -> Doc
ppSection = text . linuxSectionNum

-- JAS escape the text
escape :: Bool -> String -> String
escape _ [] = []
escape escapeSpace (c:cs)
    | c `elem` ("\t\'\"-" ++ if escapeSpace then " " else []) = ['\\', c] ++ (escape escapeSpace cs)
    | otherwise = c : (escape escapeSpace cs)



ppStr :: Bool -> String -> Doc
ppStr escapeSpace = text . escape escapeSpace

ppElement :: Element -> Doc
ppElement (Paragraph PlainParagraph) = text ".PP"
ppElement (Paragraph (IndentedParagraph mDesignator mNNN)) =
    text ".IP" <+> case mDesignator of
                     Nothing -> empty
                     (Just designator) -> text designator <+> case mNNN of
                                                                Nothing -> empty
                                                                (Just nnn) -> ppNNN nnn
ppElement (Paragraph (HangingParagraph mNNN)) =
    text ".HP" <+> maybe empty ppNNN mNNN
ppElement (Paragraph (TP mNNN label)) =
    text ".TP" <+> maybe empty ppNNN mNNN $+$ ppText False label
ppElement (Section heading) =
    text ".SH" <+> ppText True heading
ppElement (SubSection heading) =
    text ".SS" <+> ppText True heading
ppElement (Font font) =
    text ".ft" <+> case font of
                     R -> text "R"
                     B -> text "B"
                     I -> text "I"
                     CW -> text "CW"
                     P -> text "P"
ppElement (RS mNNN) =
    text ".RS" <+> maybe empty ppNNN mNNN
ppElement (RE mNNN) =
    text ".RE" <+> maybe empty ppNNN mNNN
ppElement Break =
    text ".br"
ppElement Empty =
    empty
ppElement (Text' txt) =
    ppText False txt

ppText :: Bool -> Text -> Doc
ppText escapeSpace (Text txts) = hcat (map ppTxt txts)
    where
      ppTxt (Str str) = ppStr escapeSpace str
      ppTxt (Escape' e) = ppEscape e

ppEscape :: Escape -> Doc
ppEscape (EFont R) = text "\\fR"
ppEscape (EFont B) = text "\\fB"
ppEscape (EFont I) = text "\\fI"
ppEscape (EFont CW) = text "\\f(CW"
ppEscape (EFont P) = text "\\fP"

ppNNN :: NNN -> Doc
ppNNN (DefaultUnit i) = int i
