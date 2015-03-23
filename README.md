# Lishp

[![Build Status](https://travis-ci.org/wildlyinaccurate/lishp.svg?branch=master)](https://travis-ci.org/wildlyinaccurate/lishp)

A tiny Lisp interpreter written in Haskell.

## Installation

```
git clone https://github.com/wildlyinaccurate/lishp.git
cd lishp
cabal install
make
```

## Usage

Lishp can be run as a REPL

```
$ ./bin/lishp
lishp=> (/ 8 (+ 1 1))
4
```

Or it can take programs directly

```
$ ./bin/lishp '(/ 8 (+ 1 1))'
4
```

## Tests

Tests are currently written with [roundup(1)](http://bmizerany.github.io/roundup/). Run the suite with `make test`.

## Why?

Because after playing around with Haskell for some time, I wanted to write a "real" program. I'm a big fan of Lisps, and was inspired by projects like [Make a Lisp](https://github.com/kanaka/mal), [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), and [wisp](https://github.com/walpurgisriot/wisp).
