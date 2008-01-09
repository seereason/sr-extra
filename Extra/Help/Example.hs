
ex3 = cw <> text "hello " <> b <> text "world " <> p <> text "zoink"


ex4 = section (text "first section") <> 
       ((pp <> text "whee" <> br <> rs Nothing ex3)) <>
      subsection (text "first sub section")  <>
      (text "sub section text that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on ")


ex5 = Man (title "my title" General) ex4

test = writeFile "test.1"  ((show (ppMan ex5)) ++ "\n")

data Opt 
    = Verbose
    | Count String
    | Rar (Maybe String)

specs :: [OptDescr Opt]
specs = 
    [ Option ['v','V'] ["verbose"] (NoArg Verbose) "Say quite a bit."
    , Option ['c','C'] ["count","num"] (ReqArg Count "NUM") "c is for cookie."
    , Option ['r'] ["rar"] (OptArg Rar "RAR") "rar!!!"
    ]


ex2 =
    Manpage { name = "foo"
            , sectionNum = General
            , shortDesc = text "a program that does something"
            , synopsis = text "rock and roll"
            , description = text "description"
            , options = Just specs
            , extraSections = Nothing
            , files = Just [("/etc/foo.conf", text "global config file for foo.")]
            , environment = Just [ ("FOOCONF",text "alternative location for global foo.conf")
                                 , ("FOOVERBOSE",text "default verbosity level for foo.")
                                 ]
            , diagnostics = Just (text ("0 on success. 1 on failure."))
            , bugs = Just (text "This program is bug-free.")
            , authors = Just [("Jeremy Shaw", "jeremy@example.org")
                             , ("neko paws", "neko@example.org")
                             ]
            , seeAlso = Just [("bar", General)
                             ,("baz", SystemAdministration)
                             ]

            }



-- indent :: Maybe NNN -> [Element] -> Element


{-
toIText (Text' (Str t) : Break : es) =
    toIText (IText (t
-}


{-
    case eToI es of
      (IText strs : rest) -> 
-}
{-
eToI ((Text' t) : es) =
    case (t2t t, eToI es) of
      
    where
      t2t (Text elms) = IText [convert elms]
      convert [] = []
      convert (Str s : ts) = s ++ convert ts
      convert (Escape' (EFont _) : ts) = convert ts
-}



{-

To print text, we:

 1. strip out font information
 2. join adjacent text sections
 3. print the text segment with fsep

Multiple line breaks in a row do not add blank lines. A break at the
beginning of a section does not add blank lines. A break just breaks.

 1. use vcat to print Text segments that are separated by breaks

Paragraphs:

 1. A paragraph can only contain text, break, font. Anything else
 terminates the paragraph.

 2. Anything can go inside a RS except section/subsection

-}

{-
manToDoc :: Man -> Doc
manToDoc (Man mTitle body) =
    elementsToDoc body

elementsToDoc :: Elements -> Doc
elementsToDoc (Elements elements) = 
    undefined (normalize elements)
-}

{-
normalize :: [Element] -> [Element]
normalize [] = []
normalize (Font _ : es) = normalize es
normalize (Text' t : es) = 
    case normalize es of
      (Text' u : fs) = normalizeText (Text' (t ++ u)) : fs
-}
