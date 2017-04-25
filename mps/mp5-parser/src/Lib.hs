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
realIdent = do t <- ident <?> "an identifier"
               _ <- inlinews
               return (Symbol t)

-- epsilon - parse "eps" or "ε" returning Epsilon
epsilon :: Parser Symbol
epsilon = do _ <- (string "ε" <|> string "eps") <?> "an epsilon"
             _ <- inlinews
             return Epsilon

-- epsilonLine - parse a production that only has an epsilon in it.
epsilonLine :: Parser [Symbol]
epsilonLine = do _ <- epsilon
                 _ <- endOfLine
                 return [Epsilon]

-- tokenLine - parse a production that is not an epsilon.
tokenLine :: Parser [Symbol]
tokenLine = do tt <- many1 realIdent
               _ <- endOfLine
               return tt

-- initialProduction - parse an initial production line
initialProduction :: Parser (String,[Symbol])
initialProduction = do s <- ident
                       _ <- stringws "->"
                       tt <- epsilonLine <|> tokenLine
                       return (s,tt)

continueProduction :: Parser [Symbol]
continueProduction = do try $ do _ <- inlinews
                                 stringws "|"
                        res <- epsilonLine <|> tokenLine
                        return res

production :: Parser Production
production = do (s,x) <- initialProduction
                xs <- many continueProduction
                return (Production s (x:xs))

grammar :: Parser Grammar
grammar = do p <- many1 production
             return (Grammar p (terminals p) (nonTerminals p))

p0 = "S -> x S y\n | z q Y\n"
p1 = "S -> x S y\n | z q Y\nY -> x Y y \n| eps\n"

-- Some analysis

nonTerminals :: [Production] -> S.HashSet Symbol
nonTerminals g = S.fromList $ map (\ (Production s _) -> Symbol s) g

symbols :: [Production] -> S.HashSet Symbol
symbols [] = S.empty
symbols ((Production s xx):ps) = S.union (S.insert (Symbol s) $ S.fromList (concat xx)) (symbols ps)

terminals :: [Production] -> S.HashSet Symbol
terminals g = S.difference (symbols g) (nonTerminals g)

--symbols_help :: [[Symbol]] -> [Production] -> S.HashSet Symbol
--symbols_help [] xx = symbols xx
--symbols_help ((x:xc):xs) xx = S.insert x (symbols_help (xc:xs) xx)

fix f x =
  if x == result
    then x
    else fix f result
  where result = f x

-- getFirstSet grammar
-- calculate the first sets of the nonterminals in a grammar

getFirstSet :: Grammar -> H.HashMap Symbol (S.HashSet Symbol)
getFirstSet (Grammar psets terminals nonterminals) = 
  fix aux initial
  where initial = H.fromList (zip (S.toList nonterminals) (repeat S.empty))
        aux fs = getfirst_help fs psets

getfirst_help :: H.HashMap Symbol (S.HashSet Symbol) -> [Production] -> H.HashMap Symbol (S.HashSet Symbol)
getfirst_help fs [] = H.empty
getfirst_help fs ((Production s x):xs) =
                  case H.lookup (Symbol s) fs of
                      Just v ->
                          H.insert (Symbol s) (getfirst_help2 fs v x) (getfirst_help fs xs)
                      _ -> getfirst_help fs xs

getfirst_help2 :: H.HashMap Symbol (S.HashSet Symbol) -> S.HashSet Symbol -> [[Symbol]] -> S.HashSet Symbol
getfirst_help2 fs v [] = S.empty
getfirst_help2 fs v (x:xs) =
                    S.union (S.union (first fs x) v) (getfirst_help2 fs v xs)

        -- idea aux fs = H.update k (S.union H.lookup Just Cur (first x sx)

-- first fs symbols
-- return the first set of a set of symbols
first :: H.HashMap Symbol (S.HashSet Symbol) -> [Symbol] -> S.HashSet Symbol
first s [] = S.empty
first s (x:xs) 
    | H.member x s = S.union (S.union (S.fromList [x]) (first_helpper s (x:xs))) (first_esp s (x:xs))
    | otherwise = S.fromList [x]

--first_helps
first_helpper :: H.HashMap Symbol (S.HashSet Symbol) -> [Symbol] -> S.HashSet Symbol
first_helpper s (x:xs) =
          let Just hhSet = H.lookup x s
              set_result = first_help hhSet xs
          in set_result
                where first_help hhSet xs
                          | S.member Epsilon hhSet = S.union (S.delete Epsilon hhSet) (first s xs)
                          | otherwise = hhSet

first_esp :: H.HashMap Symbol (S.HashSet Symbol) -> [Symbol] -> S.HashSet Symbol
first_esp s [] = S.fromList [Epsilon]
first_esp s (x:xs) =
    case H.lookup x s of
        Just v ->
            if S.member Epsilon v
            then first_esp s xs
            else S.empty
        _ -> S.empty
        
-- isLL
isLL :: Grammar -> Bool
isLL (Grammar psets terminals nonterminals) =
    let first_set = getFirstSet (Grammar psets terminals nonterminals)
        result = (isLL_help first_set psets) && not (isCommon psets nonterminals S.empty)
    in result

isLL_help :: H.HashMap Symbol (S.HashSet Symbol) -> [Production] -> Bool
isLL_help first_set [] = True
isLL_help first_set ((Production s _ ):xs) =
    case H.lookup (Symbol s) first_set of
        Just v ->
            if S.member (Symbol s) v
            then False
            else isLL_help first_set xs
        _ -> isLL_help first_set xs

isCommon :: [Production] -> S.HashSet Symbol -> S.HashSet Symbol -> Bool
isCommon [] _ _ = False
isCommon ((Production s ((r:x):xx)):xs) nt hs =
        if S.member r hs
        then True
        else isCommon ((Production s (xx)):xs) nt (S.insert r hs) 
isCommon ((Production s _):xs) nt _ = isCommon xs nt S.empty
