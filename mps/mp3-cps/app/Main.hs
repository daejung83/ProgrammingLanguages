--- Given Code
--- ==========

module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

fromParse :: Either ParseError Exp -> Exp
fromParse (Right exp) = exp
fromParse (Left err)  = error $ show err

ctorParse :: String -> String
ctorParse = ctorShow . fromParse . parseExp

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Integer
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Integer)

var :: Parser String
var = let keywords = ["if", "then", "else"]
      in  try $ do v1 <- letter                  <?> "an identifier"
                   vs <- many (letter <|> digit) <?> "an identifier"
                   spaces
                   let v = v1:vs
                   if (any (== v) keywords)
                    then fail "keyword"
                    else return v

oper :: Parser String
oper = do op <- many1 (oneOf "+-*/<>=") <?> "an operator"
          spaces
          return op

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

--- ### Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: String -> Parser (Exp -> Exp -> Exp)
opExp str = do symbol str
               return (OpExp str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = opExp "*" <|> opExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = opExp "+" <|> opExp "-"

compOp :: Parser (Exp -> Exp -> Exp)
compOp = try (opExp "<=") <|> try (opExp ">=")
         <|> opExp "<"    <|> opExp ">"
         <|> opExp "/="   <|> opExp "=="

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           return $ IfExp e1 e2 e3

lamExp :: Parser Exp
lamExp = do try $ symbol "\\"
            param <- var
            symbol "->"
            body <- expr
            return $ LamExp param body

atom :: Parser Exp
atom =     intExp
       <|> ifExp
       <|> lamExp
       <|> varExp
       <|> parens expr

expr :: Parser Exp
expr = let arith  = term `chainl1` addOp
           term   = factor `chainl1` mulOp
           factor = app
           app    = do f <- many1 atom
                       return $ foldl1 AppExp f
       in  arith `chainl1` compOp

parseExp :: String -> Either ParseError Exp
parseExp str = parse expr "stdin" str

--- ### Declarations

decl :: Parser Stmt
decl = do f <- var
          params <- many1 var
          symbol "="
          body <- expr
          return $ Decl f params body

parseDecl :: String -> Either ParseError Stmt
parseDecl str = parse decl "stdin" str

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            _      -> do case parseDecl input of
                            Left err    -> do printLn "Parse error!"
                                              printLn $ show err
                            Right decl  -> printLn . show $ cpsDecl decl
                         repl


main :: IO ()
main = do putStrLn "Welcome to the CPS Transformer!"
          repl
          putStrLn "GoodBye!"


--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk p k = 
    if p > 1
      then factk (p-1) (\res -> k $ p * res)
      else k 1

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk (x:xs) k ko =
  if (x `mod` 2 == 0)
    then if xs == []
            then k x
            else evenoddk xs (\res -> k (x + res)) ko
    else if xs == []
            then ko x
            else evenoddk xs k (\res -> ko $ x + res)

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`
{-
data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)
-}
isSimple :: Exp -> Bool
isSimple (AppExp a b) = False
isSimple (IntExp a) = True
isSimple (VarExp a) = True
isSimple (LamExp a b) = isSimple b 
isSimple (IfExp a b c) = isSimple a && isSimple b && isSimple c
isSimple (OpExp a b c) = isSimple b && isSimple c

--- ### `cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)`

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k gen = (AppExp k (IntExp i), gen)
cpsExp (VarExp v) k gen = (AppExp k (VarExp v), gen)

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k gen 
      | isSimple e1 = let (a, b) = cpsExp e2 k gen
                          (c, d) = cpsExp e3 k b
                      in (IfExp e1 a c , d)
      | otherwise = let (v, next) = gensym gen
                        (a, b) = cpsExp e2 k next
                        (c, d) = cpsExp e3 k b
                    in cpsExp e1 (LamExp v (IfExp(VarExp v) a c)) d
              

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k gen
      | isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2), gen)
      | not (isSimple e1) && isSimple e2 = 
                                let (v, gen2) = gensym gen
                                in cpsExp e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) gen2
      | isSimple e1 && not (isSimple e2) =
                                let (v, gen2) = gensym gen
                                in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) gen2
      | not (isSimple e1) && not (isSimple e2) =
                                let (v1, gen1) = gensym gen
                                    (v2, gen2) = gensym gen1
                                    (r1, g1) = cpsExp e2 (LamExp v2 (AppExp k(OpExp op (VarExp v1) (VarExp v2)))) gen2
                                in cpsExp e1 (LamExp v1 r1) g1

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp e1 e2) k gen
-- | isSimple e2 = (AppExp (AppExp k e2) e1, gen)
      | isSimple e2 = ((AppExp (AppExp e1 e2) k), gen)
      | not (isSimple e2) = 
                let (v, g1) = gensym gen
                in cpsExp e2 (LamExp v (AppExp  (AppExp e1 (VarExp v)) k)) g1

--data Stmt = Decl String [String] Exp
--- ### Define `cpsDecl`
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl str sarr ex) =
    let karr = sarr++["k"]
        k = VarExp "k"
        (ns, en) = cpsExp ex k 0
    in Decl str karr ns





