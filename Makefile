.SILENT:

.PHONY: grammar install clean

default: build

FILE ?= ""

RUN_ARGS =
ifneq (${FILE}, "")
	RUN_ARGS = exes -- ${FILE}
endif

run: grammar
	cabal run ${RUN_ARGS}

test: grammar
	cabal test

build: grammar
	echo "Building cabal project..."
	cabal build

grammar:
	bnfc -d grammar/Lang.cf -o grammar >> /dev/null

install:
	cabal update
	cabal install alex happy BNFC --overwrite-policy=always

clean: 
	rm -rf grammar/Lang
	cabal clean
