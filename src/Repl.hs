module Repl (repl) where

import Expr
import Parser
import Pretty
import Simplify
import System.Console.Haskeline
import Text.Megaparsec.Error

repl :: IO ()
repl = runInputT defaultSettings (loop 0)
  where
    loop :: Int -> InputT IO ()
    loop n = do
      minput <- getInputLine ("In["++show n++"]=")
      case minput of
        Nothing -> outputStrLn "Quitting..." -- Ctrl+D
        Just input -> case words input of
          [] -> loop n
          [c] | c `elem` [":q", ":quit"] -> outputStrLn "Quitting..."
              | c `elem` [":h", ":help"] -> do
                outputStrLn "TODO: Help message"
                loop n
          (c:rest) | c `elem` [":p", ":print"] -> do
            case parser (unwords rest) of
              Left err -> outputStrLn ("Parse error: " ++ errorBundlePretty err)
              Right expr -> outputStrLn . show $ expr
            loop (n + 1)
          ((':':_):_) -> do
            outputStrLn "Unknown command (try :h[elp])"
            loop n
          _ -> do
            run input automaticSimplify
            loop (n + 1)

run :: String -> (Expr -> Expr) -> InputT IO ()
run str f = case parser str of
  Left err -> outputStrLn ("Parse error: " ++ errorBundlePretty err)
  Right expr -> outputStrLn . pretty . f $ expr
