text:  foo.1
	groff foo.1 -t -e -mandoc -Tascii | col -bx

foo.1: Help.hs
	runhaskell -i../../ Help.hs

ps: foo.1
	groff foo.1 -t -e -mandoc > foo.ps
