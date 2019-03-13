--Testejä jäsentimelle
module MesserTests where

import Data.Maybe
import System.Directory
import Test.Tasty
import Test.Tasty.HUnit
import Messer

ajaTestit = do
    testit <- testit
    defaultMain testit

testit :: IO TestTree
testit = do 
    testit <- parseOnnistui
    return (testGroup "Jäsentimen testit" [testit])

tpf :: String
tpf = "../../TestiKoodit/"

--Kaikki tiedostot annetusta kansiosta
testitiedostot :: IO [String]
testitiedostot = listDirectory tpf

--Testaa jäsentyvätkö kaikki testitiedostot
parseOnnistui :: IO TestTree
parseOnnistui = do
    tiedostot <- testitiedostot
    testit <- jTestit tiedostot
    return (testGroup "Jäsentäminen onnistui" testit)

jTestit :: [String] -> IO [TestTree]
jTestit [] = return []
jTestit (x:xs) = do
    rest <- jTestit xs
    input <- tTiedosto x
    if (dropWhile (/= '.') x) == ".mess"
        then return ((testCase (x ++ " jäsentyy") $ assertBool (x ++ " ei jäsenny") input) : rest)
        else return rest

tTiedosto :: String -> IO Bool
tTiedosto tiedosto = do 
            tulos <- testaaTiedosto (tpf ++ tiedosto)
            return (isJust tulos)