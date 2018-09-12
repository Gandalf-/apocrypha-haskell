
list: *.hs Apocrypha/*.hs Devbot/*.hs
		stack ghc -- --make Main.hs -o list

clean:
	-rm *.hi *.o
