module Main where

import Messer
import MesserTests
import Interpreter
import Messemlyzer
import Data.Maybe
import Data.Either
import Text.Megaparsec

main :: IO ()
main = do
    --ajaTestit
    --iTest
    --ast <- testaaTiedosto "test.mess"
    ast <- runParser jasennin "test.mess" <$> readFile "test.mess"
    either (putStr <$> errorBundlePretty) analyzeInterpret ast
    {-if isRight ast 
        then do
            let ast2 = analysoi (fromRight ast)
            either interpret print ast2

        else print (fromLeft ast)-}

analyzeInterpret :: Luokka -> IO()
analyzeInterpret luokka = do
    let ast = analysoi luokka
    either interpret (putStr <$> unlines) ast
