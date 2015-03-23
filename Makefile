ROUNDUP ?= 'roundup'

all:: build

build: lishp copy

lishp:
	cabal build

copy:
	[ -d bin ] || mkdir bin
	cp dist/build/lishp/lishp bin/lishp

test: build
	LISHP=dist/build/lishp/lishp $(ROUNDUP) test/*
