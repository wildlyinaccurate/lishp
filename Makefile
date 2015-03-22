all:: build

build:
	cabal build

test: build
	LISHP=dist/build/lishp/lishp roundup test/*
