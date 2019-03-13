--Staattinen tyypintarkastus ja pientä optimointia 
module Messemlyzer where

import Messer 
import qualified Data.Map.Strict as Map

----------------------- Tietotyypit -------------------------------------

--Analyysi palauttaa tämän tyyppisen rakenteen, jossa aliohjelmaympäristö on valmiiksi rakennettuna tulkkia varten
data ALuokka = ALuokka Id MainOhjelma Aliohjelmat deriving (Show)

type AVirhe = String

--aliohjelmaympäristö
type Aliohjelmat = Map.Map String (Palautustyyppi, [Parametri], [Lauseke])

data AArvo = AInt | ABool | AString | AVoid deriving (Show, Eq)

--muuttujaympäristö
type AMuuttujat = Map.Map String AArvo

-------------------------- Analyysi -------------------------------------

-- kokoaa eri osasten semanttisen tarkastuksen
analysoi :: Luokka -> Either ALuokka [AVirhe]
analysoi (Luokka id main xs) = let 
                        a = mapAliohj xs
                        (uusiYmp, avirheet) = aAliohjelmat xs a
                        (uusiMain, virheet) = aMain main uusiYmp
                        in if (avirheet ++ virheet) == [] 
                                then Left (ALuokka id uusiMain uusiYmp)
                                else Right (avirheet++virheet)

-- Tekee aliohjelmalistasta aliohjelma Mapin, eli aliohjelmaympäristön
-- Käytetään aliohjelmien tarkastukseen
mapAliohj :: [Aliohjelma] -> Aliohjelmat
mapAliohj [] = Map.empty
mapAliohj ((Aliohjelma (Id id) t ps ls):xs) = (Map.singleton id (t, ps, ls)) <> mapAliohj xs

-- Tutkii onko aliohjelmassa tyyppivirheitä
-- Palauttaa optimoidut aliohjelmat ympäristössä ja tarkastuksessa mahdollisesti ilmenneet virheet
-- Palautettua ympäristöä käytetään mainohjelman tarkastukseen ja annetaan myös eteenpäin tulkille
aAliohjelmat :: [Aliohjelma] -> Aliohjelmat -> (Aliohjelmat, [AVirhe])
aAliohjelmat [] a = (a, [])
aAliohjelmat ((Aliohjelma (Id id) t ps ls):xs) a = let
                                        palautustyyppi = case t of
                                            Void -> AVoid
                                            Palautustyyppi TTInt -> AInt
                                            Palautustyyppi TTBool -> ABool
                                            Palautustyyppi TTString -> AString
                                        uusiYmp = uYmparisto ps <> Map.singleton "1return" palautustyyppi --lisätään aliohjelman palautustyyppi muuttujaympäristöön nimellä, joka ei voi ola muuten muuttujan nimenä. Käytetään palautustyypin tarkastamiseen kun kohdataan return-lause
                                        (lausekkeet, avirheet, palautusYmp) = aLausekkeet ls uusiYmp a   
                                        aotPalautus = Map.singleton id (t, ps, lausekkeet)
                                        in (aotPalautus, avirheet) <> aAliohjelmat xs a

                                                

-- Luo uuden muuttujaympäristön rekursiivisesti
-- Muuttujaympäristöön tallennetaan muuttujien tyypit tyypintarkastusta varten
uYmparisto :: [Parametri] -> AMuuttujat
uYmparisto [] =  Map.empty
uYmparisto ((Parametri tt (Id id)):ps) = Map.singleton id arvo <> uYmparisto ps
                            where  
                                arvo  = case tt of
                                    TTInt -> AInt
                                    TTBool -> ABool
                                    TTString -> AString  
                      

aMain :: MainOhjelma -> Aliohjelmat -> (MainOhjelma, [AVirhe])
aMain (MainOhjelma params xs) a = (MainOhjelma params lausekkeet, virheet)
                            where
                                (lausekkeet, virheet, _) = aLausekkeet xs m a
                                m = uYmparisto params

-- Tarkistaa lausekkeiden tyyppien paikkansapitävyydet
-- palauttaa lausekkeet joita yksinkertaistaa, virheet ja muuttujaympäristön,
-- joka sisältää paluuarvon jos sellainen on
aLausekkeet :: [Lauseke] -> AMuuttujat -> Aliohjelmat -> ([Lauseke], [AVirhe], AMuuttujat)
aLausekkeet [] m a = ([], [], m)
aLausekkeet ((LEhto (If maar xs)):rest) m a = let                                       --ehtolausekkeet ja silmukat tarkastetaan jo tällä tasolla, jotta voidaan tarkastaa erikseen tilanteet, joissa ehto on tosi ja epätosi
                                (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                virhe2 = case tyyppi of
                                    Just TTBool -> []
                                    _ -> ["If-lauseen ehdon tulee tuottaa Bool"]
                                (uxs, virheet, um) = aLausekkeet xs m a
                                (truelausekkeet, truevirheet, truem) = aLausekkeet rest um a --vaihtoehto, jossa ehto on tosi
                                (falselausekkeet, falsevirheet, falsem) = aLausekkeet rest m a --vaihtoehto, jossa ehto on epätosi
                                in ((LEhto (If umaar uxs)):truelausekkeet, virhe1++virhe2++virheet++truevirheet++falsevirheet, truem) -- yhdistetään virheet kaikista vaihtoehdoista
aLausekkeet ((LEhto (IfElse maar xs ys)):rest) m a = let
                                (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                virhe2 = case tyyppi of
                                    Just TTBool -> []
                                    _ -> ["If-lauseen ehdon tulee tuottaa Bool"]
                                (uxs, virheet1, um1) = aLausekkeet xs m a
                                (uys, virheet2, um2) = aLausekkeet ys m a
                                (truelausekkeet, truevirheet, truem) = aLausekkeet rest um1 a --vaihtoehto, jossa ehto on tosi
                                (falselausekkeet, falsevirheet, falsem) = aLausekkeet rest um2 a --vaihtoehto, jossa ehto on epätosi
                                in ((LEhto (IfElse umaar uxs uys)):truelausekkeet, virhe1++virhe2++virheet1++virheet2++truevirheet++falsevirheet, truem)  -- yhdistetään virheet kaikista vaihtoehdoista
aLausekkeet ((LSilmukka maar xs):rest) m a = let
                                (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                virhe2 = case tyyppi of
                                    Just TTBool -> []
                                    _ -> ["Silmukan ehdon tulee tuottaa Bool"]
                                (uxs, virheet, um) = aLausekkeet xs m a
                                (truelausekkeet, truevirheet, truem) = aLausekkeet rest um a --vaihtoehto, jossa ehto on tosi
                                (falselausekkeet, falsevirheet, falsem) = aLausekkeet rest m a --vaihtoehto, jossa ehto on epätosi
                                in ((LSilmukka umaar uxs):truelausekkeet, virhe1++virhe2++virheet++truevirheet++falsevirheet, truem) -- yhdistetään virheet kaikista vaihtoehdoista
aLausekkeet (x:rest) m a = let
                (lauseke, virheet1, um) = aLauseke x m a
                (ulausekkeet, virheet2, uum) = aLausekkeet rest um a
                in (lauseke:ulausekkeet, virheet1 <> virheet2, uum) 

--Tarkistaa yksittäisen lausekkeen 
aLauseke :: Lauseke -> AMuuttujat -> Aliohjelmat -> (Lauseke, [AVirhe], AMuuttujat)
aLauseke (LTulostus maar) m a = let
                    (_, umaar, virheet) = aMaaritelma maar m a
                    in (LTulostus umaar, virheet, m) 
aLauseke (LSijoitus (UusiSijoitus t (Id id) maar)) m a =let 
                                                            (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                                            (uusim, virhe2) = case Map.lookup id m of 
                                                                Just _ -> (m, ["Muuttuja " ++ id ++ " on jo määritelty"])
                                                                Nothing -> case t of        --lisätään uusi muuttuja ympäristöön
                                                                    TTInt -> (Map.insert id (AInt) m, [])
                                                                    TTBool -> (Map.insert id (ABool) m, [])
                                                                    TTString -> (Map.insert id (AString) m, [])
                                                        in case tyyppi of
                                                            Just tyyppiJust -> if (tyyppiJust == t)
                                                                then (LSijoitus (UusiSijoitus t (Id id) umaar), virhe1++virhe2, uusim)
                                                                else (LSijoitus (UusiSijoitus t (Id id) umaar), virhe1++virhe2++["Muuttujan " ++ id ++ " piti olla " ++ show t ++ ", mutta oli " ++ show tyyppiJust ++ "."], uusim)
                                                            Nothing -> (LSijoitus (UusiSijoitus t (Id id) umaar), virhe1++virhe2++["Muuttujaan "++ id ++" sijoitettavan arvon tyyppiä ei voitu määrittää. Onko se void?"], uusim)
aLauseke (LSijoitus (VanhaSijoitus (Id id) maar)) m a = let 
                                                            (tyyppi, umaar, virhe1) = aMaaritelma maar m a
                                                            rLauseke = LSijoitus (VanhaSijoitus (Id id) umaar)
                                                            in case Map.lookup id m of
                                                                Just (AInt) -> if(tyyppi == Just TTInt)
                                                                                    then (rLauseke, virhe1, m)
                                                                                    else (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"Int\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just (ABool) ->if(tyyppi == Just TTBool)
                                                                                    then (rLauseke, virhe1, m)
                                                                                    else (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"Bool\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just (AString) ->if(tyyppi == Just TTString)
                                                                                    then (rLauseke, virhe1, m)
                                                                                    else (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"String\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++"."], m)
                                                                Just AVoid -> (rLauseke, virhe1++["Muuttujan " ++ id ++ " tyyppi on \"Void\", mutta siihen yritettiin sijoittaa " ++ show tyyppi ++". Jotain on pahasti pielessä o__O"], m)
                                                                Nothing -> (rLauseke, virhe1++["Muuttujaa " ++ id ++ " ei ole määritelty."], m)
aLauseke (LPalautus maar) m a = let 
                                virhe1 = case Map.lookup "return" m of
                                            Just _ -> ["Aliohjelmassa voi olla vain yksi palautuslause."]
                                            Nothing -> []
                                (t, umaar, virhe2) = aMaaritelma maar m a
                                tyyppi = case t of
                                    Just TTInt -> AInt
                                    Just TTString -> AString
                                    Just TTBool -> ABool
                                virhe3 = case Map.lookup "1return" m of --aliohjelman palautustyyppi on tallennettu muuttujaympäristöön nimellä "1return"
                                            Just r ->  if (r == tyyppi)
                                                        then []
                                                        else ["Aliohjelman palautustyyppi on "++show r++" mutta yritettiin palauttaa "++show tyyppi]
                                            Nothing -> ["Main-ohjelmasta ei voi palauttaa mitään"]
                                in (LPalautus umaar, virhe1++virhe2++virhe3, Map.singleton "return" tyyppi <> m) --palautustyyppi lisätään muuttujaympäristöön  nimellä, joka ei siellä voi olla muussa tapauksessa
aLauseke (LAKutsu (AKutsu (Id id) xs)) m a = case Map.lookup id a of
                                Just (_, params, _) -> (LAKutsu (AKutsu (Id id) xs), aParametrit id xs params m a, m)
                                Nothing -> (LAKutsu (AKutsu (Id id) xs), ["Aliohjelmaa "++id++" ei ole määritelty"], m)


--Tarkastaa yhden määritelmän tyypin ja mahdollisesti optimoi
aMaaritelma :: Maaritelma -> AMuuttujat -> Aliohjelmat -> (Maybe Tietotyyppi, Maaritelma, [AVirhe])                                                           
aMaaritelma (MId (Id id)) m a = case Map.lookup id m of 
                            Just AInt -> (Just TTInt, MId (Id id), [])   
                            Just ABool -> (Just TTBool, MId (Id id), [])    
                            Just AString -> (Just TTString, MId (Id id), []) 
                            Just AVoid -> (Nothing, MId (Id id), ["Muuttujan " ++ id ++ " tyyppi on void, tämän ei pitäisi olla sallittua"])
                            Nothing    -> (Nothing, MId (Id id), ["Muuttujaa " ++ id ++ " ei ole määritelty"])
aMaaritelma (MArvo arvo) m a = case arvo of
                            ArvoInt _ -> (Just TTInt, MArvo arvo, []) 
                            ArvoString _ -> (Just TTString, MArvo arvo, [])  
                            ArvoBool _ -> (Just TTBool, MArvo arvo, [])
aMaaritelma (Aritmeettinen op (MArvo (ArvoInt luku1)) (MArvo (ArvoInt luku2))) m a = case op of --jos löytyy aritmeettinen operaatio, jonka molemmat operandit ovat lukuja, sievennetään lasku pelkäksi luvuksi. Ei valitettavasti toimi toistaiseksi rekursiivisesti, eli jos on useita sisäkkäisiä/peräkkäisiä laskutoimituksia vain yksi sievennetään
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
aMaaritelma (Vertailu op (MArvo (ArvoInt luku1)) (MArvo (ArvoInt luku2))) m a = case op of --jos löytyy vertailuoperaatio, jonka molemmat operandit ovat lukuja, sievennetään vertailu pelkäksi totuusarvoksi. Ei valitettavasti toimi toistaiseksi rekursiivisesti, eli jos on useita sisäkkäisiä/peräkkäisiä vertailuja vain yksi sievennetään
                            Pienempi -> (Just TTBool, MArvo (ArvoBool (luku1 < luku2)), [])       
                            Suurempi -> (Just TTBool, MArvo (ArvoBool (luku1 > luku2)), [])  
                            PienempiYhtasuuri -> (Just TTBool, MArvo (ArvoBool (luku1 <= luku2)), [])  
                            SuurempiYhtasuuri -> (Just TTBool, MArvo (ArvoBool (luku1 >= luku2)), [])    
                            Yhtasuuri -> (Just TTBool, MArvo (ArvoBool (luku1 == luku2)), [])     
                            Erisuuri -> (Just TTBool, MArvo (ArvoBool (luku1 /= luku2)), [])      
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

--Tarkistaa annetaanko aliohjelmakutsussa oikea määrä parametreja ja ovatko ne oikean tyyppisiä
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
