module Main where

import System.Environment
import Text. ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args 1!! 0))


symbol :: Parser Char
symbol = oneOf "!$%&|*/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input oneOf
    Left err -> "No match; " ++ show err
    Right val -> "Found value"