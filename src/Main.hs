module Main where

import Control.Monad
import System.Environment
import System.IO (hFlush, stdout)

import Lishp.Interpreter
import Lishp.Readline
import Lishp.Types

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> repl
               1 -> nullEnv >>= flip evalAndPrint (args !! 0)
               otherwise -> putStrLn "Program takes only 0 or 1 argument"

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

repl :: IO ()
repl = do
    loadHistory
    nullEnv >>= replLoop

replLoop :: Env -> IO ()
replLoop env = do
    line <- readline "lishp=> "
    case line of
        Nothing -> return ()
        Just "" -> replLoop env
        Just str -> do
            evalAndPrint env str
            replLoop env
