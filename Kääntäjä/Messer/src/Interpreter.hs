-- Tulkkaa Messerin antamat rakenteet

-- Katsoimme alustavaa mallia seuraavasta linkistä (ensimmäinen vastaus): https://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell 
-- Toteutus on kuitenkin muuttunut paljon alustavasta versiosta
module Interpreter where

import Messer
import Messemlyzer hiding (AArvo, I, B, S)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Text.Read

----------------------- Tietotyypit -------------------------------------

data IArvo = I Int | B Bool | S String | Void deriving (Show)

type Muuttujat = Map.Map String IArvo

---------------------------- Tulkki -------------------------------------

interpret :: [String] -> ALuokka -> IO ()
interpret args (ALuokka _ main amap) = iMain args main amap

iMain :: [String] -> MainOhjelma -> Aliohjelmat -> IO()
iMain args (MainOhjelma params xs) a = do
                    let m = mainParams params args
                    lausekkeet xs m a
                    print "Ohjelman suoritus onnistui"

--Tarkistaa oliko komentoriviparametreja oikea määrä, tarkistaa niiden tyypit ja ne muuttujaympäristöön jos kaikki oli ok
mainParams :: [Parametri] -> [String] -> Muuttujat
mainParams [] [] = Map.empty
mainParams (x:xs) [] = error "Ohjelma tarvitsee enemmän komentoriviparametreja"
mainParams [] (y:ys) = error "Ohjelmalle on annettu liikaa komentoriviparametreja"
mainParams ((Parametri t (Id id)):xs) (y:ys) = case t of
                                TTInt -> case readMaybe y :: Maybe Int of
                                        Just a -> Map.singleton id (I a) <> mainParams xs ys
                                        _ -> error("Komentoriviparametrin "++ id ++ " piti olla tyyppiä Int, mutta ei ollut")
                                TTBool -> case readMaybe y :: Maybe Bool of
                                        Just a -> Map.singleton id (B a) <> mainParams xs ys
                                        _ -> error("Komentoriviparametrin "++ id ++ " piti olla tyyppiä Bool, mutta ei ollut")
                                TTString -> Map.singleton id (S y) <> mainParams xs ys


-- Suorittaa lausekkeet 
exec :: Lauseke -> Muuttujat -> Aliohjelmat -> IO Muuttujat
exec (LTulostus x) m a = do
                evalprint <- eval x m a
                case evalprint of
                    I i -> print i
                    B b -> print b
                    S s -> print s
                    Interpreter.Void -> return ()
                return m
exec (LSijoitus x) m a = case x of
                UusiSijoitus _ (Id id) y -> do
                                    arvo <- eval y m a
                                    return (Map.insert id arvo m)
                VanhaSijoitus (Id id) y -> do
                                    arvo <- eval y m a
                                    return (Map.insert id arvo m)
exec (LPalautus x) m a = do
                arvo <- eval x m a
                return (Map.singleton "return" arvo) --tallennetaan palautusarvo ympäristöön nimellä, joka ei voi olla muuten käytössä (return on avainsana eikä voi olla muuttujan tms nimenä)
exec (LEhto lause) m a = case lause of
                If ehto xs -> do
                            -- lasketaan ehdon arvo ja katsotaan että se on bool
                            evalehto <- eval ehto m a
                            case evalehto of
                                -- Jos ehto on totta suoritetaan lausekkeet ja 
                                -- palautetaan niiden määrittämä ympäristö.
                                -- Muuten ympärisöä ei muuteta
                                B b -> do
                                    if b 
                                        then lausekkeet xs m a
                                        else return m
                                _ -> error ("If:n ehdon tyyppi ei ollut bool tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")
                IfElse ehto xs ys -> do
                                -- Muuten sama kuin yllä, mutta jos ehto on epätosi,
                                -- suoritetaan toiset lausekkeet 
                                evalehto <- eval ehto m a
                                case evalehto of
                                    B b -> do
                                        if b 
                                            then lausekkeet xs m a
                                            else lausekkeet ys m a
                                    _ -> error ("IfElse:n ehdon tyyppi ei ollut bool tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")
exec (LSilmukka ehto xs) m a = do
                evalehto <- eval ehto m a
                case evalehto of
                    B b -> do
                        if b 
                            then do
                                uusiM <- lausekkeet xs m a
                                exec (LSilmukka ehto xs) uusiM a
                            else 
                                return m
                    _ -> error ("Silmukan ehdon tyyppi ei ollut bool tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")
exec (LAKutsu (AKutsu (Id id) xs)) m a = case Map.lookup id a of
                        Nothing -> error ("Aliohjelmaa "++id++" ei löydy tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")
                        Just (t, param, ys) -> do
                                    parametrit <- params xs param m a Map.empty
                                    lausekkeet ys parametrit a
                                    return m


-- Suorittaa kaikki listan lausekkeet
lausekkeet :: [Lauseke] -> Muuttujat -> Aliohjelmat -> IO Muuttujat
lausekkeet [] m a = return m
lausekkeet (x:xs) m a = do
                uusiM <- exec x m a
                lausekkeet xs uusiM a



--tulkkaa määritelmiä
eval :: Maaritelma -> Muuttujat -> Aliohjelmat -> IO IArvo
eval (MId (Id x)) m a = case Map.lookup x m of
                        Nothing -> error ("Muuttujaa " ++ x ++ " ei ole määritelty tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")
                        Just y  -> return y
eval (MArvo x) m a = case x of 
                        ArvoInt y -> return (I y)
                        ArvoBool y -> return (B y)
                        ArvoString y -> return (S y)
eval (Aritmeettinen op x y) m a  = case op of
                        Plus -> do
                            arvo1 <- eval x m a
                            arvo2 <- eval y m a
                            return (I ((iluku arvo1) + (iluku arvo2)))
                        Miinus -> do
                            arvo1 <- eval x m a
                            arvo2 <- eval y m a
                            return (I ((iluku arvo1) - (iluku arvo2)))
                        Kerto -> do
                            arvo1 <- eval x m a
                            arvo2 <- eval y m a
                            return (I ((iluku arvo1) * (iluku arvo2)))
                        Jako -> do
                            arvo1 <- eval x m a
                            arvo2 <- eval y m a
                            return (I (div (iluku arvo1) (iluku arvo2)))
eval (Vertailu vop x y) m a = case vop of
                        Pienempi          -> do
                                arvo1 <- eval x m a
                                arvo2 <- eval y m a
                                return (B ((iluku arvo1) < (iluku arvo2)))
                        Suurempi          -> do
                                arvo1 <- eval x m a
                                arvo2 <- eval y m a
                                return (B ((iluku arvo1) > (iluku arvo2)))
                        PienempiYhtasuuri -> do
                                arvo1 <- eval x m a
                                arvo2 <- eval y m a
                                return (B ((iluku arvo1) <= (iluku arvo2)))
                        SuurempiYhtasuuri -> do
                                arvo1 <- eval x m a
                                arvo2 <- eval y m a
                                return (B ((iluku arvo1) >= (iluku arvo2)))
                        Yhtasuuri         -> do
                                arvo1 <- eval x m a
                                arvo2 <- eval y m a
                                return (B ((iluku arvo1) == (iluku arvo2)))
                        Erisuuri          -> do
                                arvo1 <- eval x m a
                                arvo2 <- eval y m a
                                return (B ((iluku arvo1) /= (iluku arvo2)))
eval (MAKutsu (AKutsu (Id id) xs)) m a = case Map.lookup id a of
                        Nothing -> error ("Aliohjelmaa ei löydy tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")
                        Just (t, param, ys) -> do
                                    parametrit <- params xs param m a Map.empty
                                    palautus <- lausekkeet ys parametrit a
                                    case Map.lookup "return" palautus of --"return"-nimiseen muuttujaan on tallennettu mahdollinen paluuarvo
                                        Nothing -> return Interpreter.Void
                                        Just arvo -> return arvo


--tekee parametreista uuden muuttujaympäristön aliohjelmaa varten
params :: [Maaritelma] -> [Parametri] -> Muuttujat -> Aliohjelmat -> Muuttujat -> IO Muuttujat
params [] [] am a um = return um
params (x:xs) ((Parametri t (Id id)):ys) am a um = do
                                arvo <- eval x am a
                                let uusiM = Map.insert id arvo um
                                params xs ys am a uusiM
params _ _ _ _ _ = error ("Parametrien määrä ei täsmää tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")


iluku :: IArvo -> Int
iluku (I x) = x
iluku e = error ("Piti olla luku, oli " ++ show e ++ " tulkkausvaiheessa, tyypintarkastuksessa on jokin vika")
