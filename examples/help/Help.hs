module Main where

import Extra.Help
import System.Console.GetOpt

-- * Test

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

testPage =
    Manpage { name = "foo" 
            , section = General
            , shortDesc = "a program that does foo" 
            , synopsis = "foo [options] arg1 arg2 ..."
            , description = "this is a very useful program"
            , options = Just specs
            , extraSections = Just [("FOO","foo"), ("BAR","bar")]
            , files = Just [("/etc/foo.conf","System wide config file.")]
            , environment = Just [("FOOCONF","Alternate location of system wide config file.")]
            , diagnostics = Just "This program may occasionally print diagnostic information on stderr."
            , bugs = Just "This program is bug-free."
            , authors = Just [ ("Jeremy Shaw","jeremy@example.org")
                             , ("Neko Paws","neko@example.org")
                             ]
            , seeAlso = Just [("bar", General), ("baz", SystemAdministration)]
            }

printPage = putStrLn $ manpageToGroff testPage
writePage = writeFile "foo.1" (manpageToGroff testPage)

main = writePage
