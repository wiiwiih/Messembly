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
    ast <- runParser jasennin (head args) <$> readFile (head args) --jäsennetään koodi komentoriviparametrina annetusta tiedostosta
    either (putStr <$> errorBundlePretty) (analyzeInterpret (drop 1 args)) ast --joko tulostetaan virheilmoitukset tai siirrytään eteenpäin (mikäli virheitä ei ollut)
    --ajaTestit -- tällä ajetaan testit tiedostosta MesserTests.hs

analyzeInterpret :: [String] -> Luokka -> IO()
analyzeInterpret args luokka = do
    let ast = analysoi luokka -- staattinen tyypintarkastus ja vähän optimointia
    either (interpret args) (putStr <$> unlines) ast --joko tulostetaan virheet tai tulkataan ohjelma (mikäli virheitä ei ollut)
