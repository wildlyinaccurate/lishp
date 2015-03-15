module Main where

import Lishp.Evaluator
import Lishp.Parser
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
