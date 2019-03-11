module Main where

import Messer
import MesserTests
import Interpreter
import Messemlyzer
import Data.Maybe
import Data.Either
import Text.Megaparsec
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    ast <- runParser jasennin (head args) <$> readFile (head args)
    either (putStr <$> errorBundlePretty) (analyzeInterpret (drop 1 args)) ast
    --ajaTestit -- tällä ajetaan testit tiedostosta MesserTests.hs

analyzeInterpret :: [String] -> Luokka -> IO()
analyzeInterpret args luokka = do
    let ast = analysoi luokka
    either (interpret args) (putStr <$> unlines) ast
