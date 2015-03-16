all:: build

build: lishp
	echo "Built lishp at 'bin/lishp'"

lishp:
	cd src && ghc --make -o ../bin/lishp Main

test: build
	roundup test/*
