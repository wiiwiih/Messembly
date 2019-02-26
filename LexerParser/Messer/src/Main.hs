module Main where

import Messer
import MesserTests
import Interpreter
import Messemlyzer
import Data.Maybe
import Data.Either

main :: IO ()
main = do
    --ajaTestit
    --iTest
    ast <- testaaTiedosto "test.mess"
    if isJust ast 
        then do
            let ast2 = analysoi (fromJust ast)
            either interpret print ast2

        else print "rip"
