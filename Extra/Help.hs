module Extra.Help where

import Data.List
import System.Console.GetOpt

data Manpage a
    = Manpage { name 	  	:: String
              , section		:: Section
              , shortDesc 	:: String
              , synopsis	:: String
              , description	:: String
              , options		:: Maybe [OptDescr a]
              , extraSections   :: Maybe [(String, String)]
              , files		:: Maybe [(FilePath, String)]
              , environment	:: Maybe [(String, String)]
              , diagnostics	:: Maybe String
              , bugs		:: Maybe String
              , authors		:: Maybe [(String, String)]
              , seeAlso		:: Maybe [(String, Section)]
              }

data Section 
    = CLibHeaders
    | General
    | SystemCalls
    | CLibraryFunctions
    | SpecialFiles
    | FileFormats
    | GamesAndScreensavers
    | Miscellanea
    | SystemAdministration
    | Kernel 
    | TclTk
    | XWindowSystem
      deriving (Show, Read, Eq)

-- TODO: add string as required escaping
manpageToGroff :: Manpage a -> String
manpageToGroff manpage =
    unlines (concat 
             [[ ".TH " ++ name manpage ++ " " ++ linuxSectionNum (section manpage)
              , ".SH NAME"
              , (name manpage) ++ " \\- " ++ (shortDesc manpage)
              , ".SH SYNOPSIS"
              , synopsis manpage
              , ".SH DESCRIPTION"
              , ".B " ++ name manpage
              , description manpage
              ]
             , maybe [] gFormatOpts (options manpage) 
             , maybe [] gFormatExtraSections (extraSections manpage)
             , maybe [] gFormatFiles (files manpage) 
             , maybe [] gFormatEnvironment (environment manpage)
             , maybe [] gFormatDiagnostics (diagnostics manpage)
             , maybe [] gFormatBugs (bugs manpage)
             , maybe [] gFormatAuthors (authors manpage)
             , maybe [] gFormatSeeAlso (seeAlso manpage)
             ])

gFormatOpts :: [OptDescr a] -> [String]
gFormatOpts opts =
    [".SH OPTIONS"] ++ (concatMap formatOpt opts)
    where
      formatOpt :: OptDescr a -> [String]
      formatOpt (Option shortOpts longOpts argDescr optionDescr) =
          [".TP"
          , concat (intersperse (",") $ 
                (map (\c -> "\\fB \\-" ++ [c] ++ (argString short)) shortOpts ++
                 map (\opt -> "\\fB \\-\\-" ++ opt ++ (argString long)) longOpts))
          , optionDescr
          ]
          where
            argString long = 
                case argDescr of
                  NoArg _ -> []
                  ReqArg _ arg ->  concat [if long then "=" else " ", "\\fI" ++ arg ++ "\\fP"]
                  OptArg _ arg ->  concat [if long then "[=" else "[","\\fI" ++ arg, "\\fP]"]
            short = False
            long = True

gFormatExtraSections :: [(String, String)] -> [String]
gFormatExtraSections sections =
    concatMap gFormatExtraSection sections
    where
      gFormatExtraSection (name, contents) =
          [".SH " ++ name 
          , contents
          ]


gFormatFiles :: [(FilePath, String)] -> [String]
gFormatFiles files =
    [".SH FILES"] ++ concatMap formatFile files
    where
      formatFile (filename, description) =
          [".I " ++ filename
          ,".RS"
          ,description
          ]

gFormatEnvironment :: [(String, String)] -> [String]
gFormatEnvironment vars =
    [".SH ENVIRONMENT VARIABLES"
    ,"The following environment variables affect the behaviour of the program."
    ,".PP"
    ] ++ concatMap formatVar vars
    where
      formatVar (name, description) =
          [".I " ++ name
          ,".RS"
          ,description
          ]

gFormatDiagnostics :: String -> [String]
gFormatDiagnostics diag =
    [".SH DIAGNOSTICS"
    , diag 
    ]

gFormatBugs :: String -> [String]
gFormatBugs bugs =
    [".SH BUGS"
    , bugs
    ]

gFormatAuthors :: [(String, String)] -> [String]
gFormatAuthors authors =
    [ if (singleton authors) then ".SH AUTHOR" else ".SH AUTHORS"
    ] ++ concatMap formatAuthor authors
    where
      singleton [_] = True
      singleton _ = False
      formatAuthor (author, email) = 
          [".LP ", author ++ " <" ++ email ++ ">"]

gFormatSeeAlso :: [(String, Section)] -> [String]
gFormatSeeAlso pages =
    [ ".SH SEE ALSO" ] ++ foldr gFormatPage [] pages
    where
      gFormatPage :: (String, Section) -> [String] -> [String]
      gFormatPage (name, section) also =
          (".BR " ++ name ++ "(" ++ linuxSectionNum section ++ ")" ++ 
           if (null also) then ([]) else ",") : also


-- | 
-- BSD and Linux are the same, System V has some differences.
--
linuxSectionNum :: Section -> String
linuxSectionNum CLibHeaders = "0"
linuxSectionNum General = "1"
linuxSectionNum SystemCalls = "2"
linuxSectionNum CLibraryFunctions = "3"
linuxSectionNum SpecialFiles = "4"
linuxSectionNum FileFormats = "5"
linuxSectionNum GamesAndScreensavers = "6"
linuxSectionNum Miscellanea = "7"
linuxSectionNum SystemAdministration = "8"
linuxSectionNum Kernel = "9"
linuxSectionNum TclTk = "n"
linuxSectionNum XWindowSystem = "x"

