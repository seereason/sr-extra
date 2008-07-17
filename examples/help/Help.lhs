> module Main where

Writing help text and manpages for our internal tools was never our strong 
point on the OS team at Linspire. I recall one day Cliff wanted to use a tool
that Sean wrote, and so he sagely ran:

  $ isostream --help

His optimism was rewarded with:

 'This is where the help would be if I had written any.'

In other cases, there was --help text, but it referred to options that
did not exist anymore, and did not include new options that did
exist. There were also many complaints about how many columns wide
should the help text be.

The only thing less common than good help text, was a manpage.

So, I now present to you, 6 years too late, the solution to all our
help and manpage problems. This could is still in alpha, so that
means there are definately some bugs, and the API is likely to have
some minor breakage. That means it is not too late for you to suggest
changes in the form of darcs patches. :)

The source currently lives inside the haskell-extra module at:

darcs get http://src.seereason.com/haskell-extra

It likely depends on some other modules also hosted there, such as
haskell-ugly.

To use the incredible new system, you need only to import the
Extra.Help module. However, we will also import 'withArgs' for 
testing purposes.

> import Extra.Help
> import System.Environment (withArgs)

First we create a data type for the options our program will take:

> data Opt 
>     = Verbose
>     | Count String
>     | Rar (Maybe String)
>     | Help
>     | DumpManPage
>       deriving (Eq, Show, Read)

then we create an option specification that details the various
options. This looks exactly like the 'OptDescr a' type from
System.Console.GetOpt, except that the last argument is prefaced by
'text' instead of just being a plain old string.

> optionSpecs :: [OptDescr Opt]
> optionSpecs = 
>     [ Option ['v','V'] ["verbose"]       (NoArg Verbose)      (text "Say quite a bit.")
>     , Option ['c','C'] ["count","num"]   (ReqArg Count "NUM") (text <> b <> text "c" <> p <> text " is for cookie.")
>     , Option ['r']     ["rar"]           (OptArg Rar "RAR")   (text "rar!!!")
>     , Option ['h']	 ["help"]	   (NoArg Help)         (text "Show this help text and exit immediately.")
>     , Option []	 ["dump-man-page"] (NoArg DumpManPage)  (text "Show manpage source and exit immediately.")
>     ]

> manpage :: String -> Manpage Opt
> manpage progName =
>     Manpage { name          = progName
>             , sectionNum    = General
>             , shortDesc     = text "a program that does foo" 
>             , synopsis      = text "foo [options] arg1 arg2 ..."
>             , description   = text "this is a very useful program"
>             , options       = Just optionSpecs
>             , extraSections = Nothing -- Just [("FOO","foo"), ("BAR","bar")]
>             , files         = Nothing -- Just [("/etc/foo.conf", text "System wide config file.")]
>             , environment   = Nothing -- Just [("FOOCONF","Alternate location of system wide config file.")]
>             , diagnostics   = Just (text "This program may occasionally print diagnostic information on stderr.")
>             , bugs          = Just (text "This program is bug-free.")
>             , authors = Just [ ("Jeremy Shaw","jeremy@example.org")
>                              , ("Neko Paws","neko@example.org")
>                              ]
>             , seeAlso = Just [("bar", General), ("baz", SystemAdministration)]
>             }

> main =
>     do (opts, nonOpts) <- getOptions (elem Help) (elem DumpManPage) manpage Permute optionSpecs analyze
>        print (opts, nonOpts)
>     where
>       analyze (opts,[filename],[]) = Right (opts, filename)
>       analyze (opts,args,[]) = Left ["Too many files or not enough."]
>       analyze (_,_,errors)   = Left errors
