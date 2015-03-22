# Lishp

A tiny Lisp interpreter written in Haskell.

## Prerequisites

```
cabal install random list control.monad.writer mtl parsec readline
```

## Installation

```
git clone https://github.com/wildlyinaccurate/lishp.git
cd lishp
make
```

## Usage

```
$ ./bin/lishp "(+ 1 1)"
2
```

## Tests

Tests are currently written with [roundup(1)](http://bmizerany.github.io/roundup/). Run the suite with `make test`.

## Why?

Because after playing around with Haskell for some time, I wanted to write a "real" program. I'm a big fan of Lisps, and was inspired by projects like [Make a Lisp](https://github.com/kanaka/mal), [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), and [wisp](https://github.com/walpurgisriot/wisp).
