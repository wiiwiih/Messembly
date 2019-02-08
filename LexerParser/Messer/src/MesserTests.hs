module MesserTests where

import Test.Tasty
import Test.Tasty.HUnit
import Messer

ajaTestit = defaultMain testit

testit :: TestTree
testit = testGroup "Jäsentimen testit" [parseOnnistui]

tpf :: String
tpf = "..\\..\\..\\TestiKoodit\\"

parseOnnistui :: TestTree
parseOnnistui = testGroup "Jäsentäminen onnistui" 
                [ testCase "simpleArith jäsentyy" $ assertBool "simpleArith ei jäsenny" (tTiedosto "simpleArith.mess")

                ]

tTiedosto tiedosto = do 
            tulos <- testaaTiedosto (tpf + tiedosto)
            case tulos of
                    Right _ -> return True
                    Left _ -> return False