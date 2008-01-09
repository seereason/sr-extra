module Extra.Help.DSL
    ( (<>)
    , text
    , b, i, r, cw, p, font
    , pp, lp, ip, hp, tp
    , br
    , section, subsection
    , title
    , indent, rs
    , manpageToMan
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


manpageToMan :: Manpage a -> Man
manpageToMan manpage =
             Man (Just (Title (name manpage) (sectionNum manpage) Nothing))
                     ((Elements ([ Section (Text [Str "NAME"])
                                , name'
                                , Section (Text [Str "SYNOPSIS"])
                                , Text' $ synopsis manpage
                                , Section (Text [Str "DESCRIPTION"])
                                , Text' $ description manpage
                                ]) <>
                       optionSection (options manpage) <>
                       formatFiles (files manpage) <>
                       formatEnvironment (environment manpage) <>
                       optional formatDiagnostics (diagnostics manpage) <>
                       optional formatBugs (bugs manpage) <>
                       optional formatAuthors (authors manpage) <>
                       optional formatSeeAlso (seeAlso manpage)
                      ))
    where
      name' = 
          let (Text elms) = shortDesc manpage
          in
            Text' (Text ((Str $ name manpage) : (Str " - ") : elms))
      optionSection :: Maybe [OptDescr a] -> Elements
      optionSection Nothing = Elements []
      optionSection (Just opts) = section (text "OPTIONS") <> (formatOptions opts)
      formatOptions :: [OptDescr a] -> Elements
      formatOptions options = flatten $ map formatOption options

      unElements (Elements es) = es
      formatOption :: OptDescr a -> Elements
      formatOption (Option shortOpts longOpts argDescr optionDescr) = 
          tp Nothing (punctuate (text ", ") 
                      ((map (\c -> b <> text ("-" ++ [c]) <> (argString argDescr short)) shortOpts) ++
                       (map (\opt -> b <> text ("--" ++ opt) <> (argString argDescr long)) longOpts))) <>
          (text optionDescr)
      punctuate :: Text -> [Text] -> Text
      punctuate _ [] = Text []
      punctuate p txts = flattenText (intersperse p txts)
      flattenText [] = Text []
      flattenText txts = Text (concatMap unText txts)
      unText (Text elms) = elms
      argString argDescr long = 
          case argDescr of
            NoArg _ -> text ""
            ReqArg _ arg -> (if long then (text "=")  else (text " ")) <> i <> text arg <> p
            OptArg _ arg -> (if long then (text "[=") else (text "[")) <> i <> text arg <> p <> text "]"
      short = False
      long = True
      formatFiles Nothing = Elements []
      formatFiles (Just files) =
          section (text "FILES") <> flatten (map formatFile files)
          where
            formatFile :: (FilePath, Elements) -> Elements
            -- formatFile (name, descr) = i <> text name <> p <> rs Nothing descr
            formatFile (name, descr) = tp Nothing (i <> text name) <> descr
      formatEnvironment Nothing = Elements []
      formatEnvironment (Just vars) =
          section (text "ENVIRONMENT VARIABLES") <>
                  (text "The following environment variables affect the behaviour of the program." <> pp <> flatten (map formatVar vars))
      formatVar :: (String, Elements) -> Elements
      formatVar (name, descr) = tp Nothing (b <> text name) <> descr
      flatten :: [Elements] -> Elements
      flatten es = Elements (concatMap unElements es)
      formatDiagnostics body =
          section (text "DIAGNOSTICS") <> body
      formatBugs body =
          section (text "BUGS") <> body
      formatAuthors :: [(String, String)] -> Elements
      formatAuthors authors =
          section (if (singleton authors) then (text "AUTHOR") else (text "AUTHORS")) <>
                  flatten (map formatAuthor authors)
          where
            singleton [_] = True
            singleton _ = False
            formatAuthor (name, email) =
                lp <> (text (name ++ " <" ++ email ++ ">"))
      formatSeeAlso :: [(String, Section)] -> Elements
      formatSeeAlso references =
          section (text "SEE ALSO") <>
                  Elements [Text' (flattenText (intersperse (text ", ") (map formatReference references)))]
          where
            formatReference (name, section) =
                b <> text name <> r <> text ("(" ++ linuxSectionNum section ++ ")")
      optional _ Nothing = Elements []
      optional f (Just v) = f v
