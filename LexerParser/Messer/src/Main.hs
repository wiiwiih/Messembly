-- Lexer ja parser Messemblylle MegaParsecilla toteutettuna
-- HUOM! WIP
-- Apuna käytetty Mark Karpovin tutoriaalia: https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
module Main where

import Control.Applicative hiding (many)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Luokka = Luokka Id MainOhjelma [Aliohjelma] deriving (Show)

data MainOhjelma = MainOhjelma Parametri [Lauseke] deriving (Show)

data Aliohjelma = Aliohjelma Id Palautustyyppi [Parametri] [Lauseke] deriving (Show)

data Id = Id String deriving (Show)

data Palautustyyppi = Palautustyyppi Tietotyyppi | Void deriving (Show)

data Tietotyyppi = TTInt | TTBool | TTString | TTVoid deriving (Show)

data Parametri = Parametri Tietotyyppi Id deriving (Show)

data Arvo = ArvoInt Int | ArvoBool Bool | ArvoString String deriving (Show)

data Lauseke = LTulostus Maaritelma
             | LSijoitus Sijoitus 
             | LPalautus Maaritelma
             | LEhto Ehtolause
             | LSilmukka Maaritelma [Lauseke]
             | LAKutsu AKutsu deriving (Show)

data Maaritelma = MId Id
                | MArvo Arvo 
                | Aritmeettinen Op Maaritelma Maaritelma
                | Vertailu VOp Maaritelma Maaritelma
                | MAKutsu AKutsu deriving (Show)

data AKutsu = AKutsu Id [Maaritelma] deriving (Show)

data Sijoitus = UusiSijoitus Tietotyyppi Id Maaritelma 
              | VanhaSijoitus Id Maaritelma deriving (Show)

data Ehtolause = If Maaritelma [Lauseke] | IfElse Maaritelma [Lauseke] [Lauseke] deriving (Show)

data Op = Plus | Miinus | Kerto | Jako deriving (Show)

data VOp = Pienempi | Suurempi | PienempiYhtasuuri | SuurempiYhtasuuri | Yhtasuuri | Erisuuri deriving (Show)

type Parser = Parsec Void String

-- poistaa whitespacen 
whitespace :: Parser ()
whitespace = L.space space1 lineCmnt empty
    where
        lineCmnt = L.skipLineComment "#"

-- poistaa whitespacen jokaisen lekseemin lopusta
lekseemi :: Parser a -> Parser a
lekseemi = L.lexeme whitespace

-- poistaa whitespacen jokaisen symbolin lopusta
symboli :: String -> Parser String
symboli = L.symbol whitespace

-- pilkkoo sulkujen sisällön syötteestä
sulut :: Parser a -> Parser a
sulut = between (symboli "(") (symboli ")")

-- pilkkoo aaltosulkujen sisällön syötteestä
asulut :: Parser a -> Parser a
asulut = between (symboli "{") (symboli "}")

-- varmistaa onko seuraavana tulossa luku ja ottaa sen
integer :: Parser Int
integer = lekseemi L.decimal

-- pilkkoo merkkijonot syötteestä
--mjono :: Parser a -> Parser a
--mjono = between (symboli "\"") (symboli "\"")

-- pilkkoo kysymysmerkit syötteestä
km :: Parser String
km = symboli "?"

semi :: Parser String
semi = symboli ";"

-- varmistetaan ettei varattuja sanoja ei käytetä alkuosina id:issä
vsana :: String -> Parser ()
vsana v = (lekseemi . try) (string v *> notFollowedBy alphaNumChar)

--varattu sana lista
vsl :: [String] 
vsl = ["if","else","while","class","void","int","string","bool","true","false",
    "print", "return", "main"]

-- pilkkoo id:t syötteetstä
identifier :: Parser Id
identifier = (lekseemi . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if elem x vsl 
                    then fail $ "avainsanaa " ++ show x ++ " ei voi käyttää, koska siltä tuntuu"
                    else return (Id x)

jMain :: Parser MainOhjelma
jMain = do
    vsana "main"
    symboli "("
    vsana "string"
    symboli "[]"
    parametriNimi <- identifier
    symboli ")"
    lausekkeet <- asulut lauseke
    symboli ";"
    return (MainOhjelma (Parametri TTString parametriNimi) lausekkeet)

jAliohjelma :: Parser [Aliohjelma]
jAliohjelma = sepEndBy jAliohjelma' semi

jAliohjelma' :: Parser Aliohjelma
jAliohjelma' = do
        palautus <- palautustyyppi
        nimi <- identifier
        symboli "("
        parametrit <- parametrit
        symboli ")"
        lausekkeet <- asulut lauseke
        return (Aliohjelma nimi palautus parametrit lausekkeet)

lauseke :: Parser [Lauseke]
lauseke = sepEndBy lauseke' km

lauseke' :: Parser Lauseke
lauseke' =  tulostus
        <|> lakutsu
        <|> sijoitus
        <|> palautus
        <|> ehtolause
        <|> silmukka
        

lakutsu :: Parser Lauseke
lakutsu = do
    kutsu <- akutsu
    return (LAKutsu kutsu)

makutsu :: Parser Maaritelma
makutsu = do
    kutsu <- akutsu
    return (MAKutsu kutsu)

akutsu :: Parser AKutsu
akutsu = do
    id <- identifier
    symboli "("
    maaritelmat <- sepBy maaritelma (symboli ",")
    symboli ")"
    return (AKutsu id maaritelmat)

tulostus :: Parser Lauseke
tulostus = do
    vsana "print"
    maaritelma <- maaritelma
    return (LTulostus maaritelma)

sijoitus :: Parser Lauseke
sijoitus = uusisijoitus <|> vanhasijoitus
    
uusisijoitus :: Parser Lauseke
uusisijoitus = do
    tyyppi <- tietotyyppi 
    id <- identifier
    symboli "="
    maaritelma <- maaritelma
    return (LSijoitus (UusiSijoitus tyyppi id maaritelma))

vanhasijoitus :: Parser Lauseke
vanhasijoitus = do
    id <- identifier
    symboli "="
    maaritelma <- maaritelma
    return (LSijoitus (VanhaSijoitus id maaritelma))

palautus :: Parser Lauseke
palautus = do
    vsana "return"
    maaritelma <- maaritelma
    return (LPalautus maaritelma)

ehtolause :: Parser Lauseke
ehtolause = elsellinen <|> elseton

elsellinen :: Parser Lauseke
elsellinen = do
    vsana "if"
    ehto <- sulut maaritelma
    lausekkeet1 <- asulut lauseke
    vsana "else"
    lausekkeet2 <- asulut lauseke
    return (LEhto (IfElse ehto lausekkeet1 lausekkeet2))

elseton :: Parser Lauseke
elseton = do
    vsana "if"
    ehto <- sulut maaritelma
    lausekkeet <- asulut lauseke
    return (LEhto (If ehto lausekkeet))

silmukka :: Parser Lauseke
silmukka = do
    vsana "while"
    ehto <- sulut maaritelma
    lausekkeet <- asulut lauseke
    return (LSilmukka ehto lausekkeet)

palautustyyppi :: Parser Palautustyyppi
palautustyyppi = palautustietotyyppi <|> palautusvoid

palautustietotyyppi :: Parser Palautustyyppi
palautustietotyyppi = do
    tyyppi <- tietotyyppi
    return (Palautustyyppi tyyppi)

palautusvoid :: Parser Palautustyyppi
palautusvoid = do
    vsana "void"
    return Void

parametrit :: Parser [Parametri]
parametrit = sepBy parametri (symboli ",")

parametri :: Parser Parametri
parametri = do
    tyyppi <- tietotyyppi
    id <- identifier
    return (Parametri tyyppi id)

maaritelma :: Parser Maaritelma
maaritelma = aMaaritelma
         <|> bMaaritelma
         <|> mjono
         <|> mid
         <|> makutsu

mid :: Parser Maaritelma
mid = do
    id <- identifier 
    return (MId id)

mjono :: Parser Maaritelma
mjono = do
    symboli "\""
    merkit <- manyTill anySingle (symboli "\"")
    return (MArvo (ArvoString merkit))
    
aMaaritelma :: Parser Maaritelma
aMaaritelma = makeExprParser aTermi aOperaattorit

bMaaritelma :: Parser Maaritelma
bMaaritelma = makeExprParser bTermi bOperaattorit

aTermi :: Parser Maaritelma
aTermi = sulut aMaaritelma <|> luku <|> mid
    
luku :: Parser Maaritelma
luku = do
    arvo <- integer
    return (MArvo (ArvoInt arvo))

bTermi :: Parser Maaritelma
bTermi = sulut bMaaritelma
    <|> boolTosi
    <|> boolEtosi

boolTosi :: Parser Maaritelma
boolTosi = do
    vsana "true"
    return (MArvo (ArvoBool True))

boolEtosi :: Parser Maaritelma
boolEtosi = do
    vsana "false"
    return (MArvo (ArvoBool False))

tietotyyppi :: Parser Tietotyyppi
tietotyyppi = intti <|> booli <|> stringi 

intti :: Parser Tietotyyppi
intti = do
    vsana "int"
    return (TTInt)

booli :: Parser Tietotyyppi
booli = do
    vsana "bool"
    return (TTBool)

stringi :: Parser Tietotyyppi
stringi = do
    vsana "string"
    return (TTString)

aOperaattorit :: [[Operator Parser Maaritelma]]
aOperaattorit = [ [ InfixL (Aritmeettinen Kerto  <$ symboli "*")
                  , InfixL (Aritmeettinen Jako   <$ symboli "/") ]
                , [ InfixL (Aritmeettinen Plus   <$ symboli "+")
                  , InfixL (Aritmeettinen Miinus <$ symboli "-") ]
                ]

bOperaattorit :: [[Operator Parser Maaritelma]]
bOperaattorit = [ [ InfixL (Vertailu Pienempi          <$ symboli "<")
                  , InfixL (Vertailu Suurempi          <$ symboli ">")
                  , InfixL (Vertailu PienempiYhtasuuri <$ symboli "<=")
                  , InfixL (Vertailu SuurempiYhtasuuri <$ symboli ">=")
                  , InfixL (Vertailu Yhtasuuri         <$ symboli "==")
                  , InfixL (Vertailu Erisuuri          <$ symboli "!=") 
                ] ]

jasennin :: Parser Luokka
jasennin = between whitespace eof luokka

luokka :: Parser Luokka
luokka = do
    vsana "class"
    luokanNimi <- identifier
    symboli "{"
    maini <- jMain
    aliohjelmat <- jAliohjelma
    symboli "}"
    return (Luokka luokanNimi maini aliohjelmat)

main :: IO ()
main = do
    ioInput <- readFile "test.mess"
    parseTest jasennin ioInput










