module Repl (repl) where

import Expr
import Parser
import Simplify
import System.IO

repl :: IO ()
repl = do
  putStr "S> "
  hFlush stdout
  line <- getLine
  if line `elem` [":quit", ":q"] then putStrLn "Quitting SAS" else do
    process line
    repl

process :: String -> IO ()
process input = case words input of
  (":s":rest) ->
    run (unwords rest) automaticSimplify
  _ -> print "Unknown command"

pretty :: Expr -> String
pretty = show

run :: String -> (Expr -> Expr) -> IO ()
run str f = case parser str of
  Left err -> print err
  Right expr -> putStrLn . pretty . f $ expr
