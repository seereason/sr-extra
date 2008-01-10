module Extra.Help.Util where

import Data.List
import Data.Monoid
import Extra.Help.Man
import Extra.Help.Doc
import Extra.Help.DSL
import Extra.HughesPJ
import System.Console.GetOpt

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
                       optional optionSection (options manpage) <>
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

      formatFiles Nothing = Elements []
      formatFiles (Just files) =
          section (text "FILES") <> mconcat (map formatFile files)
          where
            formatFile :: (FilePath, Elements) -> Elements
            -- formatFile (name, descr) = i <> text name <> p <> rs Nothing descr
            formatFile (name, descr) = tp Nothing (i <> text name) <> descr
      formatEnvironment Nothing = Elements []
      formatEnvironment (Just vars) =
          section (text "ENVIRONMENT VARIABLES") <>
                  (text "The following environment variables affect the behaviour of the program." <> pp <> mconcat (map formatVar vars))
      formatVar :: (String, Elements) -> Elements
      formatVar (name, descr) = tp Nothing (b <> text name) <> descr

      formatDiagnostics body =
          section (text "DIAGNOSTICS") <> body
      formatBugs body =
          section (text "BUGS") <> body
      formatAuthors :: [(String, String)] -> Elements
      formatAuthors authors =
          section (if (singleton authors) then (text "AUTHOR") else (text "AUTHORS")) <>
                  mconcat (map formatAuthor authors)
          where
            singleton [_] = True
            singleton _ = False
            formatAuthor (name, email) =
                lp <> (text (name ++ " <" ++ email ++ ">"))
      formatSeeAlso :: [(String, Section)] -> Elements
      formatSeeAlso references =
          section (text "SEE ALSO") <>
                  Elements [Text' (punctuate (text ", ") (map formatReference references))]
          where
            formatReference (name, section) =
                b <> text name <> r <> text ("(" ++ linuxSectionNum section ++ ")")

optional :: (Monoid b) => (a -> b) -> Maybe a -> b
optional f (Just v) = f v
optional _ Nothing = mempty

optionSection :: [OptDescr a] -> Elements
optionSection opts = section (text "OPTIONS") <> (formatOptions opts)

formatOptions :: [OptDescr a] -> Elements
formatOptions options = mconcat $ map formatOption options

formatOption :: OptDescr a -> Elements
formatOption (Option shortOpts longOpts argDescr optionDescr) = 
    tp Nothing (punctuate (text ", ") 
                ((map (\c -> b <> text ("-" ++ [c]) <> (argString argDescr short)) shortOpts) ++
                 (map (\opt -> b <> text ("--" ++ opt) <> (argString argDescr long)) longOpts))) <>
           (text optionDescr)
    where
      argString argDescr long = 
          case argDescr of
            NoArg _ -> text ""
            ReqArg _ arg -> (if long then (text "=")  else (text " ")) <> i <> text arg <> p
            OptArg _ arg -> (if long then (text "[=") else (text "[")) <> i <> text arg <> p <> text "]"
      short = False
      long = True

-- JAS - use Extra.HughesPJ.renderWidth to format --help text to current terminal width
usage :: Manpage a -> IO String
usage manpage =
    renderWidth (elementsToDoc (te (synopsis manpage) <> (optional optionSection (options manpage))))

