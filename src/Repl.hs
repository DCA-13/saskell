module Repl (repl) where

import Expr
import Parser
import Pretty
import Simplify
import System.Console.Haskeline
import Text.Megaparsec.Error

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "S> "
      case minput of
        Nothing -> outputStrLn "Quitting..." -- Ctrl+D
        Just input -> case words input of
          [] -> loop
          [c] | c `elem` [":q", ":quit"] -> outputStrLn "Quitting..."
              | c `elem` [":h", ":help"] -> do
                outputStrLn "TODO: Help message"
                loop
          (c:rest) | c `elem` [":p", ":print"] -> do
            case parser (unwords rest) of
              Left err -> outputStrLn ("Parse error: " ++ errorBundlePretty err)
              Right expr -> outputStrLn . show $ expr
            loop
          ((':':_):_) -> do
            outputStrLn "Unknown command (try :h[elp])"
            loop
          _ -> do
            run input automaticSimplify
            loop

run :: String -> (Expr -> Expr) -> InputT IO ()
run str f = case parser str of
  Left err -> outputStrLn ("Parse error: " ++ errorBundlePretty err)
  Right expr -> outputStrLn . pretty . f $ expr
