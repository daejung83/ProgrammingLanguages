{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import Data.Functor.Identity

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Char

import GHC.Generics (Generic)
import Data.Hashable
import Data.List (intercalate)

-- The Types

data Symbol = Symbol String
           | Epsilon
   deriving (Eq,Generic)

data Production = Production String [[Symbol]]
   deriving Eq

data Grammar = Grammar [Production] (S.HashSet Symbol) (S.HashSet Symbol)

instance Show Symbol where
  show (Symbol s) = s
  show Epsilon = "ε"

instance Show Production where
  show (Production s xx) = aux header xx
    where header = s ++ " -> "
          padding = replicate (length header - 2) ' ' ++ "| "
          aux _ [] = ""
          aux prefix (x:xs) = prefix ++ unwords (map show x) ++ "\n" ++ aux padding xs

instance Hashable Symbol

showGrammar xx = concatMap show xx

-- The Parser

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- for testing a parser directly
run :: Parser a -> String -> a
run p s =
    case parse p "<stdin>" s of
        Right x -> x
        Left x  -> error $ show x

-- inlinews - parses spaces and tabs but not newlines
inlinews :: Parser String
inlinews = many (oneOf " \t") <?> "whitespace"

-- stringws - parses a string and consumes trailing whitespace
stringws :: String -> Parser String
stringws s = do _ <- string s
                _ <- inlinews
                return s

-- ident - parse a non-epsilon identifier, at least one upper or lowercase letter
ident :: Parser String
ident = do i <- try $ do ii <- many1 (oneOf (['a'..'z'] ++ ['A'..'Z'])) <?> "an identifier"
                         if ii == "eps" then fail "eps not expected" else return ii
           _ <- inlinews
           return i

-- realIdent - like ident, but returns it as a Symbol
realIdent :: Parser Symbol
realIdent = do i <- ident <?> "an identifier"
               _ <- inlinews
               return (Symbol i)

-- epsilon - parse "eps" or "ε" returning Epsilon
epsilon :: Parser Symbol
epsilon = do _ <- (string "ε" <|> string "eps") <?> "epsilon"
             _ <- inlinews
             return Epsilon

-- epsilonLine - parse a production that only has an epsilon in it.
epsilonLine :: Parser [Symbol]
epsilonLine = do _ <- many1 epsilon
                 _ <- endOfLine
                 return [Epsilon]

-- tokenLine - parse a production that is not an epsilon.
tokenLine :: Parser [Symbol]
tokenLine = do i <- many1 realIdent
               _ <- endOfLine
               return i

-- initialProduction - parse an initial production line
initialProduction :: Parser (String,[Symbol])
initialProduction = do try $ do s <- ident
                                _ <- stringws "->"
                                res <- tokenLine <|> epsilonLine
                                return (s, res)

continueProduction :: Parser [Symbol]
continueProduction = do try $ do _ <- inlinews
                                 _ <- stringws "|"
                                 res <- tokenLine <|> epsilonLine
                                 return res


production :: Parser Production
production = do (s, x) <- initialProduction
                xs <- many1 continueProduction
                return (Production s (x:xs))

grammar :: Parser Grammar
grammar = do p <- many1 production
             return (Grammar p (terminals p) (nonTerminals p))

p0 = "S -> x S y\n | z q Y\n"
p1 = "S -> x S y\n | z q Y\nY -> x Y y \n| eps\n"

-- Some analysis

nonTerminals :: [Production] -> S.HashSet Symbol
nonTerminals [] = S.empty
nonTerminals ((Production s _):xs) = S.insert (Symbol s) (nonTerminals xs)

symbols :: [Production] -> S.HashSet Symbol
symbols ((Production s ss):xs) = S.insert (Symbol s) (symbols_help ss xs)

terminals :: [Production] -> S.HashSet Symbol
terminals g = S.difference (symbols g) (nonTerminals g)

symbols_help :: [[Symbol]] -> [Production] -> S.HashSet Symbol
symbols_help [] xx = symbols xx
symbols_help ((x:xc):xs) xx = S.insert x (symbols_help (xc:xs) xx)

fix f x =
  if x == result
    then x
    else fix f result
  where result = f x

-- getFirstSet grammar
-- calculate the first sets of the nonterminals in a grammar
getFirstSet :: Grammar -> H.HashMap Symbol (S.HashSet Symbol)
getFirstSet (Grammar psets nonterminals terminals) = 
  fix aux initial
  where initial = H.fromList (zip (S.toList nonterminals) (repeat S.empty))
        aux fs = undefined --map first ((!) fs psets)

-- first fs symbols
-- return the first set of a set of symbols
first :: S.HashSet Symbol -> [Symbol] -> S.HashSet Symbol
first s [] = s
first s (x:xs) = undefined--s.insert x s

-- isLL
isLL :: Grammar -> Bool
isLL g = undefined

