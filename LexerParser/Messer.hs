-- Lexer ja parser Messemblylle MegaParsecilla toteutettuna
-- HUOM! WIP
-- Apuna käytetty Mark Karpovin tutoriaalia: https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
module Main (main) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Luokka = Luokka Id Aliohjelma [Aliohjelma]

data Aliohjelma = Aliohjelma Id Tietotyyppi [Parametri] [Lauseke]

data Id = Id String

data Tietotyyppi = TTInt | TTBool | TTString 

data Parametri = Parametri Tietotyyppi Id Arvo

data Arvo = ArvoInt Int | ArvoBool Bool | ArvoString String

data Lauseke = Tulostus Maaritelma
             | Sijoitus 
             | Palautus Maaritelma
             | Ehtolause
             | Silmukka

data Maaritelma = Id
                | Arvo 
                | Aritmeettinen Op Maaritelma Maaritelma
                | Vertailu VOp Maaritelma Maaritelma

data Sijoitus = UusiSijoitus Tietotyyppi Id Arvo 
              | VanhaSijoitus Id Arvo

data Ehtolause = If Maaritelma [Lauseke] | IfElse Maaritelma [Lauseke] [Lauseke]

data Silmukka = Silmukka Maaritelma [Lauseke]

data Op = Plus | Miinus | Kerto | Jako

data VOp = Pienempi | Suurempi | PienempiYhtasuuri | SuurempiYhtasuuri | Yhtasuuri | EiYhtasuuri






