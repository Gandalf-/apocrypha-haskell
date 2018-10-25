
HC     = ghc --make
FLAGS  = -Wall -Wno-type-defaults -Wno-missing-signatures -Wno-name-shadowing
FLAGS += -O -threaded -dynamic


all: server

server: Apocrypha/server.hs Apocrypha/Database.hs Apocrypha/Network.hs
	${HC} ${FLAGS} Apocrypha/server.hs -o server

clean:
	-find . -name '*.o'  -exec rm '{}' ';'
	-find . -name '*.hi' -exec rm '{}' ';'
