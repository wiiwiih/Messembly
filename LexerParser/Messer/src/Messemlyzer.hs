module Messemlyzer where

import Messer hiding (Void)
import qualified Data.Map.Strict as Map

data ALuokka = ALuokka Id MainOhjelma Aliohjelmat

type AVirhe = String

type Aliohjelmat = Map.Map String (Palautustyyppi, [Parametri], [Lauseke])

data AArvo = I Int | B Bool | S String | Void deriving (Show)

type AMuuttujat = Map.Map String AArvo

analysoi :: Luokka -> Either ALuokka [AVirhe]
analysoi (Luokka id main xs) = let 
                        (a, avirheet) = mapAliohj xs
                        (uusiMain, virheet) = aMain main a
                        in if (avirheet ++ virheet) == [] 
                                then Left (ALuokka id uusiMain a)
                                else Right virheet

-- Tekee aliohjelmalistasta aliohjelma Mapin
mapAliohj :: [Aliohjelma] -> (Aliohjelmat, [AVirhe])
mapAliohj [] = (Map.empty, [])
mapAliohj (x:xs) = aAliohjelma x <> mapAliohj xs


aAliohjelma :: Aliohjelma -> (Aliohjelmat, [AVirhe])
aAliohjelma = undefined

aMain :: MainOhjelma -> Aliohjelmat -> (MainOhjelma, [AVirhe])
aMain (MainOhjelma (Parametri t (Id id)) xs) a = (MainOhjelma (Parametri t (Id id)) lausekkeet, virheet)
                            where
                                (lausekkeet, virheet) = aLausekkeet xs m a
                                arvo  = case t of
                                    TTInt -> I 0
                                    TTBool -> B False
                                    TTString -> S ""
                                    TTVoid -> Void
                                m = Map.singleton id arvo

aLausekkeet :: [Lauseke] -> AMuuttujat -> Aliohjelmat -> ([Lauseke], [AVirhe])
aLausekkeet = undefined
                    