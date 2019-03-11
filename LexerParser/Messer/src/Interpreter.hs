-- Tulkkaa Messerin antamat rakenteet
module Interpreter where

import Messer
import Messemlyzer hiding (AArvo, I, B, S)
import Data.Maybe
import qualified Data.Map.Strict as Map

data IArvo = I Int | B Bool | S String | Void deriving (Show)

type Muuttujat = Map.Map String IArvo

interpret :: [String] -> ALuokka -> IO () -- ei käytetä vielä argumentteja
interpret args (ALuokka _ main amap) = iMain main amap

iMain :: MainOhjelma -> Aliohjelmat -> IO()
iMain (MainOhjelma (Parametri t (Id id)) xs) a = do
                    let m = Map.empty -- Tässä lisättäisiin komentoriviparametrien arvot ympäristöön
                    lausekkeet xs m a
                    print "Ohjelman suoritus onnistui"

-- Suorittaa lausekkeet 
exec :: Lauseke -> Muuttujat -> Aliohjelmat -> IO Muuttujat
exec (LTulostus x) m a = do
                evalehto <- eval x m a
                case evalehto of
                    I i -> print i
                    B b -> print b
                    S s -> print s
                    Interpreter.Void -> return ()
                return m
exec (LSijoitus x) m a = case x of
                UusiSijoitus t (Id id) y -> do
                                    arvo <- eval y m a
                                    return (Map.insert id arvo m)
                VanhaSijoitus (Id id) y -> do
                                    arvo <- eval y m a
                                    return (Map.insert id arvo m)
exec (LPalautus x) m a = do
                arvo <- eval x m a
                return (Map.singleton "return" arvo)
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
                                _ -> error ("Ehdon tyyppi ei ollut bool")
                IfElse ehto xs ys -> do
                                -- Muuten sama kuin yllä, mutta jos ehto on epätosi,
                                -- suoritetaan toiset lausekkeet 
                                evalehto <- eval ehto m a
                                case evalehto of
                                    B b -> do
                                        if b 
                                            then lausekkeet xs m a
                                            else lausekkeet ys m a
                                    _ -> error ("Ehdon tyyppi ei ollut bool")
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
                    _ -> error ("Ehdon tyyppi ei ollut bool")
exec (LAKutsu (AKutsu (Id id) xs)) m a = case Map.lookup id a of
                        Nothing -> error ("Aliohjelmaa ei löydy :(")
                        Just (t, param, ys) -> do
                                    parametrit <- params xs param m a Map.empty
                                    lausekkeet ys parametrit a
                                    return m


-- Kutsuu lausekkeen suoritusta yksi kerrallaan kaikille listassa oleville
lausekkeet :: [Lauseke] -> Muuttujat -> Aliohjelmat -> IO Muuttujat
lausekkeet [] m a = return m
lausekkeet (x:xs) m a = do
                uusiM <- exec x m a
                lausekkeet xs uusiM a



--tulkkaa määritelmiä
eval :: Maaritelma -> Muuttujat -> Aliohjelmat -> IO IArvo
eval (MId (Id x)) m a = case Map.lookup x m of
                        Nothing -> error ("Muuttujaa " ++ x ++ " ei ole määritelty")
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
                        Nothing -> error ("Aliohjelmaa ei löydy :(")
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
params _ _ _ _ _ = error ("Parametrien määrä ei täsmää")

testt :: Maaritelma
testt = Vertailu Pienempi (Aritmeettinen Kerto (MArvo (ArvoInt 2)) (MArvo (ArvoInt 3))) (MArvo (ArvoInt 1))

iluku :: IArvo -> Int
iluku (I x) = x
iluku e = error ("Piti olla luku, oli " ++ show e)
