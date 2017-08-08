HS_OPTS=-O -hidir ../build -odir ../build
HS_LINK_OPTS=-static -optl-static -optl-pthread

default: all

lazy-build: ../punter

build:
	mkdir -p ../build

clean:
	rm -f ../build/* ../punter

all: clean ../punter

../punter: build
	ghc ${HS_OPTS} ${HS_LINK_OPTS} -o ../punter Main.hs

