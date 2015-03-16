module Main where

import Lishp.Interpreter
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
