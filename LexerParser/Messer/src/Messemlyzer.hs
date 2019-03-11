module Messemlyzer where

import Messer 
import qualified Data.Map.Strict as Map

data ALuokka = ALuokka Id MainOhjelma Aliohjelmat deriving (Show)

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
aLausekkeet [] m a = ([], [], m)
aLausekkeet (x:xs) m a = let
                (lauseke, virheet1, um) = aLauseke x m a
                (ulausekkeet, virheet2, uum) = aLausekkeet xs um a
                in (lauseke:ulausekkeet, virheet1 <> virheet2, uum) 

aLauseke :: Lauseke -> AMuuttujat -> Aliohjelmat -> (Lauseke, [AVirhe], AMuuttujat)
aLauseke (LTulostus maar) m a = let
                    (_, umaar, virheet) = aMaaritelma maar m a
                    in (LTulostus umaar, virheet, m) 
aLauseke (LSijoitus (UusiSijoitus t (Id id) maar)) m a =let (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                                            uusim = case t of
                                                                TTInt -> Map.insert id (I 0) m
                                                                TTBool -> Map.insert id (B False) m
                                                                TTString -> Map.insert id (S "") m
                                                            virhe2 = case Map.lookup id m of 
                                                                Just _ -> ["Muuttuja " ++ id ++ " on jo määritelty"]
                                                                Nothing -> []
                                                        in case tyyppi of
                                                            Just tyyppiJust -> if (tyyppiJust == t)
                                                                then (LSijoitus (UusiSijoitus t (Id id) umaar), virhe1++virhe2, uusim)
                                                                else (LSijoitus (UusiSijoitus t (Id id) umaar), virhe1++virhe2++["Muuttujan " ++ id ++ " piti olla " ++ show t ++ ", mutta oli " ++ show tyyppiJust ++ "."], uusim)
                                                            Nothing -> (LSijoitus (UusiSijoitus t (Id id) umaar), virhe1++virhe2++["Muuttujaan "++ id ++" sijoitettavan arvon tyyppiä ei voitu määrittää. Onko se void?"], uusim)
aLauseke (LSijoitus (VanhaSijoitus (Id id) maar)) m a = let (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                                            rLauseke = LSijoitus (VanhaSijoitus (Id id) umaar)
                                                            in case Map.lookup id m of
                                                                Just (I intti) -> if(tyyppi == Just TTInt)
                                                                                    then (rLauseke, virhe1, m)
                                                                                    else (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"Int\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just (B booli) ->if(tyyppi == Just TTBool)
                                                                                    then (rLauseke, virhe1, m)
                                                                                    else (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"Bool\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just (S stringi) ->if(tyyppi == Just TTString)
                                                                                    then (rLauseke, virhe1, m)
                                                                                    else (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"String\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just AVoid -> (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"Void\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++". Jotain on pahasti pielessä o__O"], m)
                                                                Nothing -> (rLauseke, virhe1++["Muuttujaa " ++ id ++ " ei ole määritelty."], m)
aLauseke (LPalautus maar) m a = let 
                                virhe1 = case Map.lookup "return" m of
                                            Just _ -> ["Aliohjelmassa voi olla vain yksi palautuslause."]
                                            Nothing -> []
                                (t, umaar, virhe2) = aMaaritelma maar m a
                                in case t of
                                    Just TTInt -> (LPalautus umaar, virhe1++virhe2, Map.singleton "return" (I 0)<>m)
                                    Just TTBool -> (LPalautus umaar, virhe1++virhe2, Map.singleton "return" (B False)<>m)
                                    Just TTString -> (LPalautus umaar, virhe1++virhe2, Map.singleton "return" (S ""))
                                    _ -> (LPalautus umaar, virhe1++virhe2++["Aliohjelmasta ei voi palauttaa void-tyyppistä arvoa"], Map.singleton "return" (AVoid))
aLauseke (LEhto (If maar xs)) m a = let
                                (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                virhe2 = case tyyppi of
                                    Just TTBool -> []
                                    _ -> ["If-lauseen ehdon tulee tuottaa Bool"]
                                (uxs, virheet, um) = aLausekkeet xs m a
                                in (LEhto (If umaar uxs), virhe1++virhe2++virheet, um) --mikä ympäristö eteenpäin?
aLauseke (LEhto (IfElse maar xs ys)) m a = let
                                (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                virhe2 = case tyyppi of
                                    Just TTBool -> []
                                    _ -> ["If-lauseen ehdon tulee tuottaa Bool"]
                                (uxs, virheet1, um1) = aLausekkeet xs m a
                                (uys, virheet2, um2) = aLausekkeet ys m a
                                in (LEhto (IfElse umaar uxs uys), virhe1++virhe2++virheet1++virheet2, Map.union um1 um2) --mikä/mitkä ympäristöt eteenpäin?
aLauseke (LSilmukka maar xs) m a = let
                                (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                virhe2 = case tyyppi of
                                    Just TTBool -> []
                                    _ -> ["Silmukan ehdon tulee tuottaa Bool"]
                                (uxs, virheet, um) = aLausekkeet xs m a
                                in (LSilmukka umaar uxs, virhe1++virhe2++virheet, um)
aLauseke (LAKutsu (AKutsu (Id id) xs)) m a = case Map.lookup id a of
                                Just (_, params, _) -> (LAKutsu (AKutsu (Id id) xs), aParametrit id xs params m a, m)
                                Nothing -> (LAKutsu (AKutsu (Id id) xs), ["Aliohjelmaa "++id++" ei ole määritelty"], m)


--data AArvo = I Int | B Bool | S String | AVoid deriving (Show)

aMaaritelma :: Maaritelma -> AMuuttujat -> Aliohjelmat -> (Maybe Tietotyyppi, Maaritelma, [AVirhe])                                                           
aMaaritelma (MId (Id id)) m a = case Map.lookup id m of
                            Just (I _) -> (Just TTInt, MId (Id id), [])   
                            Just (B _) -> (Just TTBool, MId (Id id), [])    
                            Just (S _) -> (Just TTString, MId (Id id), []) 
                            Just AVoid -> (Nothing, MId (Id id), ["Muuttujan " ++ id ++ " tyyppi on void, tämän ei pitäisi olla sallittua"])
                            Nothing    -> (Nothing, MId (Id id), ["Muuttuja " ++ id ++ " ei ole määritelty"])
aMaaritelma (MArvo arvo) m a = case arvo of
                            ArvoInt _ -> (Just TTInt, MArvo arvo, []) 
                            ArvoString _ -> (Just TTString, MArvo arvo, [])  
                            ArvoBool _ -> (Just TTBool, MArvo arvo, [])
aMaaritelma (Aritmeettinen op (MArvo (ArvoInt luku1)) (MArvo (ArvoInt luku2))) m a = case op of
                            Plus -> (Just TTInt, MArvo (ArvoInt (luku1+luku2)), [])       
                            Miinus -> (Just TTInt, MArvo (ArvoInt (luku1-luku2)), [])  
                            Kerto -> (Just TTInt, MArvo (ArvoInt (luku1*luku2)), [])  
                            Jako -> (Just TTInt, MArvo (ArvoInt (div luku1 luku2)), [])           
aMaaritelma (Aritmeettinen op maar1 maar2) m a = let
                            (tyyppi1, umaar1, virhe1) = aMaaritelma maar1 m a
                            (tyyppi2, umaar2, virhe2) = aMaaritelma maar2 m a
                            in if (tyyppi1 == Just TTInt && tyyppi2 == Just TTInt)
                                    then (Just TTInt, Aritmeettinen op umaar1 umaar2, virhe1++virhe2)
                                    else (Nothing, Aritmeettinen op umaar1 umaar2, virhe1++virhe2++["Aritmeettisia operaatioita voi suorittaa vain luvuille"])
aMaaritelma (Vertailu op maar1 maar2) m a = let
                            (tyyppi1, umaar1, virhe1) = aMaaritelma maar1 m a
                            (tyyppi2, umaar2, virhe2) = aMaaritelma maar2 m a
                            in if (tyyppi1 == Just TTInt && tyyppi2 == Just TTInt)
                                    then (Just TTBool, Vertailu op umaar1 umaar2, virhe1++virhe2)
                                    else (Nothing, Vertailu op umaar1 umaar2, virhe1++virhe2++["Vertailuoperaatioita voi suorittaa vain luvuille"])
aMaaritelma (MAKutsu (AKutsu (Id id) xs)) m a = let
                            palautus = MAKutsu (AKutsu (Id id) xs)
                            in case Map.lookup id a of
                                Just (tyyppi, params, _) -> case tyyppi of
                                    Void -> (Nothing, palautus, aParametrit id xs params m a)
                                    Palautustyyppi t -> (Just t, palautus, aParametrit id xs params m a)
                                Nothing -> (Nothing, palautus, ["Aliohjelmaa "++id++" ei ole määritelty"])

aParametrit :: String -> [Maaritelma] -> [Parametri] -> AMuuttujat -> Aliohjelmat -> [AVirhe]
aParametrit id [] [] m a = []
aParametrit id [] (y:ys) m a = ["Aliohjelma "++id++" tarvitsee enemmän parametreja"]
aParametrit id (x:xs) [] m a = ["Aliohjelmalle "++id++" annetaan liikaa parametreja"]
aParametrit id (x:xs) ((Parametri tyyppi1 (Id pid)):ys) m a = let
                        (tyyppi2, umaar, virheet1) = aMaaritelma x m a
                        in case tyyppi2 of
                            Just t -> if t == tyyppi1
                                        then aParametrit id xs ys m a
                                        else ("Aliohjelman "++id++" parametrin "++pid++" tyypin piti olla "++show tyyppi1++" mutta oli "++show t):aParametrit id xs ys m a
                            Nothing -> ("Aliohjelman "++id++" parametrin "++pid++" tyypin piti olla "++show tyyppi1++" mutta sitä ei voitu määrittää"):aParametrit id xs ys m a
