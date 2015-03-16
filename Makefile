all:: build

build:
	cd src && ghc --make -o ../bin/lishp Main

test: build
	roundup test/*
