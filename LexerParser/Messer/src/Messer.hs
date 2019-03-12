-- Lexer ja parser Messemblylle MegaParsecilla toteutettuna
-- Apuna käytetty Mark Karpovin tutoriaalia: https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
module Messer where

import Control.Applicative hiding (many)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Luokka = Luokka Id MainOhjelma [Aliohjelma] deriving (Show)

data MainOhjelma = MainOhjelma [Parametri] [Lauseke] deriving (Show)

data Aliohjelma = Aliohjelma Id Palautustyyppi [Parametri] [Lauseke] deriving (Show)

data Id = Id String deriving (Show)

data Palautustyyppi = Palautustyyppi Tietotyyppi | Void deriving (Show)

data Tietotyyppi = TTInt | TTBool | TTString deriving (Show, Eq)

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

-- pilkkoo id:t syötteestä
identifier :: Parser Id
identifier = (lekseemi . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if elem x vsl 
                    then fail $ "avainsanaa " ++ show x ++ " ei voi käyttää, koska siltä tuntuu"
                    else return (Id x)

-- Seuraavaksi määritellään kieliopin mukaisesti eri osien parsiminen
-- käytännössä nämä toteutaan niin, että parsitaan kieliopin mukaisesti
-- osa kerrallaan kaikki tarvittava esimerkiksi pääohjelmaan

jMain :: Parser MainOhjelma
jMain = do
    vsana "main"
    symboli "("
    parametrit <- parametrit
    symboli ")"
    lausekkeet <- asulut lauseke
    symboli ";"
    return (MainOhjelma parametrit lausekkeet)

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

-- Tässä tarvitaan erillinen "idAlku", koska useampi rakenne voi alkaa id:llä
-- eikä <|> -operaatio osaa palautua, jos rakenteen toinen sananen ei toimikaan
-- ensimmäisellä yritetyllä parserilla.
lauseke' :: Parser Lauseke
lauseke' =  idAlku
        <|> uusisijoitus
        <|> tulostus
        <|> palautus
        <|> ehtolause
        <|> silmukka

idAlku :: Parser Lauseke
idAlku = do
    id <- identifier
    loppu <- idLoppu id
    return loppu

-- tässä hoidetaan loppuosan parsiminen uudelle <|> -operaatiolla
-- koska ei ole enää samankaltaisia alkuosia jäljelle jäävissä syötteissä
idLoppu :: Id -> Parser Lauseke
idLoppu id = lakutsu id <|> vanhasijoitus id

-- aliohjelma kutsun jäsennys
lakutsu :: Id -> Parser Lauseke
lakutsu id = do
    symboli "("
    maaritelmat <- sepBy maaritelma (symboli ",")
    symboli ")"
    return (LAKutsu (AKutsu id maaritelmat))

makutsu :: Id -> Parser Maaritelma
makutsu id = do
    symboli "("
    maaritelmat <- sepBy maaritelma (symboli ",")
    symboli ")"
    return (MAKutsu (AKutsu id maaritelmat))

tulostus :: Parser Lauseke
tulostus = do
    vsana "print"
    maaritelma <- maaritelma
    return (LTulostus maaritelma)

-- Kun määritellään uusi muuttuja ja annetaan sille arvo    
uusisijoitus :: Parser Lauseke
uusisijoitus = do
    tyyppi <- tietotyyppi 
    id <- identifier
    symboli "="
    maaritelma <- maaritelma
    return (LSijoitus (UusiSijoitus tyyppi id maaritelma))

-- Kun sijoitetaan vanhaan muuttujaan uusiarvo
vanhasijoitus :: Id -> Parser Lauseke
vanhasijoitus id = do
    symboli "="
    maaritelma <- maaritelma
    return (LSijoitus (VanhaSijoitus id maaritelma))

palautus :: Parser Lauseke
palautus = do
    vsana "return"
    maaritelma <- maaritelma
    return (LPalautus maaritelma)

ehtolause :: Parser Lauseke
ehtolause = do
    vsana "if"
    ehto <- sulut maaritelma
    lausekkeet <- asulut lauseke
    elsellinen ehto lausekkeet <|> elseton ehto lausekkeet

elsellinen :: Maaritelma -> [Lauseke] -> Parser Lauseke
elsellinen ehto lausekkeet1 = do
    vsana "else"
    lausekkeet2 <- asulut lauseke
    return (LEhto (IfElse ehto lausekkeet1 lausekkeet2))

elseton :: Maaritelma -> [Lauseke] -> Parser Lauseke
elseton ehto lausekkeet = return (LEhto (If ehto lausekkeet))

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
maaritelma = oMaaritelma
         <|> mjono
         <|> boolTosi
         <|> boolEtosi

idAlkuM :: Parser Maaritelma
idAlkuM = do
    id <- identifier
    loppu <- idLoppuM id
    return loppu

idLoppuM :: Id -> Parser Maaritelma
idLoppuM id = makutsu id <|> return (MId id)

mid :: Parser Maaritelma
mid = do
    id <- identifier 
    return (MId id)

mjono :: Parser Maaritelma
mjono = do
    symboli "\""
    merkit <- manyTill anySingle (symboli "\"")
    return (MArvo (ArvoString merkit))
    
oMaaritelma :: Parser Maaritelma
oMaaritelma = makeExprParser aTermi operaattorit -- ennen maaritelma = aTermi

aTermi :: Parser Maaritelma
aTermi = sulut oMaaritelma <|> luku <|> idAlkuM
    
luku :: Parser Maaritelma
luku = do
    arvo <- integer
    return (MArvo (ArvoInt arvo))


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

operaattorit :: [[Operator Parser Maaritelma]]
operaattorit = [ [ InfixL (Aritmeettinen Kerto  <$ symboli "*")
                  , InfixL (Aritmeettinen Jako   <$ symboli "/") ]
                , [ InfixL (Aritmeettinen Plus   <$ symboli "+")
                  , InfixL (Aritmeettinen Miinus <$ symboli "-") ],
                  [InfixL (Vertailu PienempiYhtasuuri <$ symboli "<=")
                  , InfixL (Vertailu SuurempiYhtasuuri <$ symboli ">=")
                  , InfixL (Vertailu Pienempi          <$ symboli "<")
                  , InfixL (Vertailu Suurempi          <$ symboli ">")
                  , InfixL (Vertailu Yhtasuuri         <$ symboli "==")
                  , InfixL (Vertailu Erisuuri          <$ symboli "!=") ]
                ]

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

-- Käytetään MesserTests.hs:ssä testien ajamiseen
testaaTiedosto :: String -> IO (Maybe Luokka)
testaaTiedosto tiedosto = parseMaybe jasennin <$> readFile tiedosto
