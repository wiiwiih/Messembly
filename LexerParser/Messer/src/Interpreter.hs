
module Interpreter where

import Messer
import Data.Maybe

type Ohjelma = Luokka

iTest = do 
    ast <- testaaTiedosto "test.mess"
    if isJust ast 
        then do
            let ast2 = fromJust ast
            interpret ast2

        else print "rip"

data IArvo = I Int | B Bool | S String deriving (Show)

type Muuttujat = [(String, IArvo)]

interpret :: a -> IO ()
interpret = undefined

eval :: Maaritelma -> Muuttujat -> IArvo
eval (MId (Id x)) m = case lookup x m of
                        Nothing -> error ("Muuttujaa " ++ x ++ " ei ole määritelty")
                        Just a  -> a
eval (MArvo x) m = case x of 
                        ArvoInt a -> I a
                        ArvoBool a -> B a
                        ArvoString a -> S a
eval (Aritmeettinen op x y) m = case op of
                        Plus -> I ((iluku (eval x m)) + (iluku (eval y m)))
                        Miinus -> I ((iluku (eval x m)) - (iluku (eval y m)))
                        Kerto -> I ((iluku (eval x m)) * (iluku (eval y m)))
                        Jako -> I (div (iluku (eval x m)) (iluku (eval y m)))
eval (Vertailu vop x y) m = case vop of
                        Pienempi          -> B ((iluku (eval x m)) < (iluku (eval y m)))
                        Suurempi          -> B ((iluku (eval x m)) > (iluku (eval y m)))
                        PienempiYhtasuuri -> B ((iluku (eval x m)) <= (iluku (eval y m)))
                        SuurempiYhtasuuri -> B ((iluku (eval x m)) >= (iluku (eval y m)))
                        Yhtasuuri         -> B ((iluku (eval x m)) == (iluku (eval y m)))
                        Erisuuri          -> B ((iluku (eval x m)) /= (iluku (eval y m)))


testt :: Maaritelma
testt = Vertailu Pienempi (Aritmeettinen Kerto (MArvo (ArvoInt 2)) (MArvo (ArvoInt 3))) (MArvo (ArvoInt 1))

iluku :: IArvo -> Int
iluku (I x) = x
iluku e = error ("Piti olla luku, oli " ++ show e)
