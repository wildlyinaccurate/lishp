module Main where

import Control.Monad
import System.Environment
import System.IO (hFlush, stdout)

import Lishp.Interpreter
import Lishp.Readline

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> repl
               1 -> evalAndPrint $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

repl = do
    loadHistory
    replLoop

replLoop = do
    line <- readline "lishp=> "
    case line of
        Nothing -> return ()
        Just "" -> replLoop
        Just str -> do
            evalAndPrint str
            replLoop
