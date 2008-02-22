module Extra.Help.Man where

import Extra.Help.Markup
import Extra.Help.GetOpt

data Man
    = Man (Maybe Title) Elements
      deriving (Show, Read, Eq)

-- .TH title section date source manual 
data Title = Title String Section (Maybe (String, Maybe (String, Maybe String)))
      deriving (Show, Read, Eq)

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
      deriving (Show, Read, Eq, Enum, Ord)

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

data Manpage a
    = Manpage { name 	  	:: String
              , sectionNum	:: Section
              , shortDesc 	:: Text
              , synopsis	:: Text
              , description	:: Text
              , options		:: Maybe [OptDescr a]
              , extraSections   :: Maybe [(ShowIn, Text, Elements)]
              , files		:: Maybe [(FilePath, Elements)]
              , environment	:: Maybe [(String, Elements)]
              , diagnostics	:: Maybe Elements
              , bugs		:: Maybe Elements
              , authors		:: Maybe [(String, String)]
              , seeAlso		:: Maybe [(String, Section)]
              }

data ShowIn
    = InManpage | InHelp | InBoth
      deriving (Read, Show, Eq)


showInManpage :: ShowIn -> Bool
showInManpage InManpage = True
showInManpage InBoth = True
showInManpage _ = False

showInHelp :: ShowIn -> Bool
showInHelp InHelp = True
showInHelp InBoth = True
showInHelp _ = False
