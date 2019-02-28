module Messemlyzer where

import Messer 
import qualified Data.Map.Strict as Map

data ALuokka = ALuokka Id MainOhjelma Aliohjelmat

type AVirhe = String

type Aliohjelmat = Map.Map String (Palautustyyppi, [Parametri], [Lauseke])

data AArvo = I Int | B Bool | S String | AVoid deriving (Show)

type AMuuttujat = Map.Map String AArvo

-- kokoaa eri osasten semanttisen tarkastuksen
analysoi :: Luokka -> Either ALuokka [AVirhe]
analysoi (Luokka id main xs) = let 
                        a = mapAliohj xs
                        (uusiYmp, avirheet) = aAliohjelmat xs a
                        (uusiMain, virheet) = aMain main uusiYmp
                        in if (avirheet ++ virheet) == [] 
                                then Left (ALuokka id uusiMain uusiYmp)
                                else Right virheet

-- Tekee aliohjelmalistasta aliohjelma Mapin
mapAliohj :: [Aliohjelma] -> Aliohjelmat
mapAliohj [] = Map.empty
mapAliohj ((Aliohjelma (Id id) t ps ls):xs) = (Map.singleton id (t, ps, ls)) <> mapAliohj xs

-- Tutkii onko aliohjelmassa tyyppivirheitä
aAliohjelmat :: [Aliohjelma] -> Aliohjelmat -> (Aliohjelmat, [AVirhe])
aAliohjelmat [] a = (a, [])
aAliohjelmat ((Aliohjelma (Id id) t ps ls):xs) a = let
                                        uusiYmp = uYmparisto ps
                                        (lausekkeet, avirheet, palautusYmp) = aLausekkeet ls uusiYmp a                                        
                                        onkoPalautus = Map.lookup "return" palautusYmp
                                        aotPalautus = Map.singleton id (t, ps, lausekkeet)
                                        virhelista = case onkoPalautus of
                                            Nothing -> case t of 
                                                        Void ->  [] 
                                                        (Palautustyyppi TTInt) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Void\", mutta oli \"Int\""]
                                                        (Palautustyyppi TTBool) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Void\", mutta oli \"Bool\""]
                                                        (Palautustyyppi TTString) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Void\", mutta oli \"String\""]  
                                            Just mapArvo -> case mapArvo of
                                                I intti -> case t of
                                                        (Palautustyyppi TTInt) -> []
                                                        Void ->  ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Int\", mutta oli \"Void\""]
                                                        (Palautustyyppi TTBool) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Int\", mutta oli \"Bool\""]
                                                        (Palautustyyppi TTString) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Int\", mutta oli \"String\""]  
                                           

                                                B booli
                                                        -> case t of 
                                                        (Palautustyyppi TTBool) -> []
                                                        (Palautustyyppi TTInt) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Bool\", mutta oli \"Int\""]
                                                        Void ->  ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Bool\", mutta oli \"Void\""]
                                                        (Palautustyyppi TTString) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"Bool\", mutta oli \"String\""]
                                                S stringi
                                                        -> case t of 
                                                        (Palautustyyppi TTString) -> []
                                                        (Palautustyyppi TTBool) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"String\", mutta oli \"Bool\""]
                                                        (Palautustyyppi TTInt) -> ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"String\", mutta oli \"Int\""]
                                                        Void ->  ["Aliohjelman " ++ id ++  "palautustyyppi piti olla \"String\", mutta oli \"Void\""]
                                                AVoid -> ["Palautustyypin ei pitäisi voida olla Void, muuta jos tähän päästään niin rip"]
                                        in (aotPalautus, avirheet ++ virhelista) <> aAliohjelmat xs a



-- type Aliohjelmat = Map.Map String (Palautustyyppi, [Parametri], [Lauseke])                                                    

-- Luo uuden muuttujaympäristön rekursiivisesti ja alustaa ne joillain arvoilla
-- Käytetään tyyppien tarkastukseen joten arvoilla ei ole väliä
uYmparisto :: [Parametri] -> AMuuttujat
uYmparisto [] =  Map.empty
uYmparisto ((Parametri tt (Id id)):ps) = Map.singleton id arvo <> uYmparisto ps
                            where  
                                arvo  = case tt of
                                    TTInt -> I 0
                                    TTBool -> B False
                                    TTString -> S ""  
                      

aMain :: MainOhjelma -> Aliohjelmat -> (MainOhjelma, [AVirhe])
aMain (MainOhjelma (Parametri t (Id id)) xs) a = (MainOhjelma (Parametri t (Id id)) lausekkeet, virheet)
                            where
                                (lausekkeet, virheet, _) = aLausekkeet xs m a
                                arvo  = case t of
                                    TTInt -> I 0
                                    TTBool -> B False
                                    TTString -> S ""
                                m = Map.singleton id arvo

-- Tarkistaa lausekkeiden tyyppien paikkansapitävyydet
-- palauttaa lausekkeet joita yksinkertaistaa, virheet ja muuttujaympäristön,
-- joka sisältää paluuarvon jos sellainen on
aLausekkeet :: [Lauseke] -> AMuuttujat -> Aliohjelmat -> ([Lauseke], [AVirhe], AMuuttujat)
aLausekkeet = undefined

aLauseke :: Lauseke -> AMuuttujat -> Aliohjelmat -> (Lauseke, [AVirhe], AMuuttujat)
aLauseke (LTulostus maar) m _ = (LTulostus maar, [], m) -- Tulostus toimii kaikille tyypeille suoraan aina
aLauseke (LSijoitus (UusiSijoitus t (Id id) maar)) m a =let tyyppi = aMaaritelma maar m a  
                                                            virhe = case Map.lookup id m of 
                                                                Just _ -> ["Muuttuja " ++ id ++ " on jo määritelty"]
                                                                Nothing -> []
                                                        in if (tyyppi == t)
                                                            then (LSijoitus (UusiSijoitus t (Id id) maar), virhe, m)
                                                            else (LSijoitus (UusiSijoitus t (Id id) maar), virhe ++ ["Muuttujan " ++ id ++ " piti olla " ++ show t ++ ", mutta oli " ++ show tyyppi ++ "."], m)
aLauseke (LSijoitus (VanhaSijoitus (Id id) maar)) m a = let tyyppi = aMaaritelma maar m a
                                                            rLauseke = LSijoitus (VanhaSijoitus (Id id) maar)
                                                            in case Map.lookup id m of
                                                                Just (I intti) -> if(tyyppi == TTInt)
                                                                                    then (rLauseke, [], m)
                                                                                    else (rLauseke, ["Muuttujan " ++ id ++ " tyyppi on \"Int\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just (B booli) ->if(tyyppi == TTBool)
                                                                                    then (rLauseke, [], m)
                                                                                    else (rLauseke, ["Muuttujan " ++ id ++ " tyyppi on \"Bool\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just (S stringi) ->if(tyyppi == TTString)
                                                                                    then (rLauseke, [], m)
                                                                                    else (rLauseke, ["Muuttujan " ++ id ++ " tyyppi on \"String\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just AVoid -> if(tyyppi == TTInt)
                                                                                    then (rLauseke, [], m)
                                                                                    else (rLauseke, ["Muuttujan " ++ id ++ " tyyppi on \"Void\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++". Jotain on pahasti pielessä o__O"], m)
                                                                Nothing -> (rLauseke, ["Muuttujaa " ++ id ++ " ei ole määritelty."], m)
aLauseke (LPalautus maar) m a = let 
                                virhe = case Map.lookup "return" m of
                                            Just _ -> ["Aliohjelmassa voi olla vain yksi palautuslause."]
                                            Nothing -> []
                                in case aMaaritelma maar m a of
                                    TTInt -> (LPalautus maar, virhe, Map.singleton "return" (I 0)<>m)
                                    TTBool -> (LPalautus maar, virhe, Map.singleton "return" (B False)<>m)
                                    TTString -> (LPalautus maar, virhe, Map.singleton "return" (S ""))

--data AArvo = I Int | B Bool | S String | AVoid deriving (Show)

aMaaritelma :: Maaritelma -> AMuuttujat -> Aliohjelmat -> Tietotyyppi                                                           
aMaaritelma = undefined                    