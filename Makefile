all: list bot

list: *.hs Apocrypha/*.hs Devbot/*.hs list.hs
		stack ghc -- --make list.hs -o list

bot: *.hs Apocrypha/*.hs Devbot/*.hs bot.hs
		stack ghc -- --make bot.hs -o bot

clean:
	-rm *.hi *.o
