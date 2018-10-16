module Main where
import System.Environment
import Tokenizer.Tokenizer
main :: IO ()
main = do
        args    <- getArgs
        s       <- readFile (args !! 1)
        putStr s
        print $ tokenize s [] 1 1
