module Main where
import System.Environment

main:: IO ()
main = do putStrLn("Ingrese un nombre: ")
          args <- getLine
          putStrLn("Hola, " ++ args ++ "!")