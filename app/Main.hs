module Main (main) where

import Parse
import Eval
import PrettyPrint

doFullExpr :: String -> IO ()
doFullExpr s = do
    case runEvaluator $ parseEval s of
        Left err -> putStrLn $ "*** Error: " ++ err
        Right evaluated -> putStrLn $ prettyPrint evaluated

main :: IO ()
main = putStrLn "Nothing here yet"