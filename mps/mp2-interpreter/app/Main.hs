--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Main where

import System.IO (hFlush, stdout)

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Data.Functor.Identity

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)


--- Given Code
--- ==========

--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Parser
--- ------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- for testing a parser directly
run :: Parser a -> String -> a
run p s =
    case parse p "<stdin>" s of
        Right x -> x
        Left x  -> error $ show x

-- Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var :: Parser String
var = do v <- many1 letter <?> "an identifier"
         spaces
         return v

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

-- Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

boolExp :: Parser Exp
boolExp =    ( symbol "true"  >> return (BoolExp True)  )
         <|> ( symbol "false" >> return (BoolExp False) )

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: (String -> Exp -> Exp -> Exp) -> String -> Parser (Exp -> Exp -> Exp)
opExp ctor str = symbol str >> return (ctor str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = let mulOpExp = opExp IntOpExp
        in  mulOpExp "*" <|> mulOpExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = let addOpExp = opExp IntOpExp
        in  addOpExp "+" <|> addOpExp "-"

andOp :: Parser (Exp -> Exp -> Exp)
andOp = opExp BoolOpExp "and"

orOp :: Parser (Exp -> Exp -> Exp)
orOp = opExp BoolOpExp "or"

compOp :: Parser (Exp -> Exp -> Exp)
compOp = let compOpExp s = symbol s >> return (CompOpExp s)
         in     try (compOpExp "<=")
            <|> try (compOpExp ">=")
            <|> compOpExp "/="
            <|> compOpExp "=="
            <|> compOpExp "<"
            <|> compOpExp ">"

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           symbol "fi"
           return $ IfExp e1 e2 e3

funExp :: Parser Exp
funExp = do try $ symbol "fn"
            symbol "["
            params <- var `sepBy` (symbol ",")
            symbol "]"
            body <- expr
            symbol "end"
            return $ FunExp params body

letExp :: Parser Exp
letExp = do try $ symbol "let"
            symbol "["
            params <- (do v <- var
                          symbol ":="
                          e <- expr
                          return (v,e)
                      )
                      `sepBy` (symbol ";")
            symbol "]"
            body <- expr
            symbol "end"
            return $ LetExp params body

appExp :: Parser Exp
appExp = do try $ symbol "apply"
            efn <- expr
            symbol "("
            exps <- expr `sepBy` (symbol ",")
            symbol ")"
            return $ AppExp efn exps

expr :: Parser Exp
expr = let disj = conj `chainl1` andOp
           conj = arith `chainl1` compOp
           arith = term `chainl1` addOp
           term = factor `chainl1` mulOp
           factor = atom
       in  disj `chainl1` orOp

atom :: Parser Exp
atom = intExp
   <|> funExp
   <|> ifExp
   <|> letExp
   <|> try boolExp
   <|> appExp
   <|> varExp
   <|> parens expr

-- Statements

quitStmt :: Parser Stmt
quitStmt = do try $ symbol "quit"
              symbol ";"
              return QuitStmt

printStmt :: Parser Stmt
printStmt = do try $ symbol "print"
               e <- expr
               symbol ";"
               return $ PrintStmt e

setStmt :: Parser Stmt
setStmt = do v <- var
             symbol ":="
             e <- expr
             symbol ";"
             return $ SetStmt v e

ifStmt :: Parser Stmt
ifStmt = do try $ symbol "if"
            e1 <- expr
            symbol "then"
            s2 <- stmt
            symbol "else"
            s3 <- stmt
            symbol "fi"
            return $ IfStmt e1 s2 s3

procStmt :: Parser Stmt
procStmt = do try $ symbol "procedure"
              name <- var
              symbol "("
              params <- var `sepBy` (symbol ",")
              symbol ")"
              body <- stmt
              symbol "endproc"
              return $ ProcedureStmt name params body

callStmt :: Parser Stmt
callStmt = do try $ symbol "call"
              name <- var
              symbol "("
              args <- expr `sepBy` (symbol ",")
              symbol ")"
              symbol ";"
              return $ CallStmt name args

seqStmt :: Parser Stmt
seqStmt = do try $ symbol "do"
             stmts <- many1 stmt
             symbol "od"
             symbol ";"
             return $ SeqStmt stmts

stmt :: Parser Stmt
stmt = quitStmt
   <|> printStmt
   <|> ifStmt
   <|> procStmt
   <|> callStmt
   <|> seqStmt
   <|> try setStmt

--- REPL
--- ----

repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     hFlush stdout
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main :: IO Result
main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"


--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

{-data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq) -}


eval :: Exp -> Env -> Val
--- ### Constants
justfilt (Just a) = a

oplook x y = justfilt(H.lookup x y)
intlook x = oplook x intOps
--- IntExp Int
eval (IntExp i) _ = IntVal i
--- BoolExp Bool
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env =
    if val == Nothing
      then ExnVal "No match in env"
      else justfilt(val)
  where val = H.lookup s env

--- ### Arithmetic
{-eval (IntOpExp op (IntExp e1) (IntExp e2)) env =
  case H.lookup op intOps
    Just o ->
      if op == "/" && e2 == IntVal 0
        then ExnVal "Division by 0"
        else IntVal $ o e1 e2
    Nothing -> ExnVal "Cannot lift"-}

eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just o = H.lookup op intOps
  in if(op == "/" && v2 == IntVal 0)
        then ExnVal "Division by 0"
        else liftIntOp o v1 v2
  
--- ### Boolean and Comparison Operators
eval (BoolOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just o = H.lookup op boolOps
    in liftBoolOp o v1 v2

eval (CompOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just o = H.lookup op compOps
    in liftCompOp o v1 v2

--- ### If Expressions
{-
eval (IfExp (BoolExp con) e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
    in if con == True
        then v1
        else v2

eval (IfExp (BoolOpExp op (BoolExp c1) (BoolExp c2)) e1 e2) env =
    let con = eval(op c1 c2) env
        v1 = eval e1 env
        v2 = eval e2 env
      in if con == True
          then v1
          else v2

eval (IfExp _ e1 e2) env = ExnVal "Condition is not a Bool"
-}
eval (IfExp op e1 e2) env =
  let o = eval op env
    in case o of
      BoolVal a -> 
          if a
            then eval e1 env
            else eval e2 env
      otherwise -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp arr b) env = CloVal arr b env

eval (AppExp fun_arr exp_arr) env =
  let o = eval fun_arr env
  in case o of
    CloVal arr b env ->
      evalAppExp arr exp_arr b env env
    otherwise -> ExnVal "Apply to non-closure"


--- ### Let Expressions

--LetExp [(String,Exp)] Exp

--eval (string, (eval arry), eval val) env
--eval (LetExp [] e2) env = eval e2 env
                                      
eval (LetExp a b) env = evalLetExp (LetExp a b) env env

--helper for let
evalLetExp :: Exp -> Env -> Env -> Val
evalLetExp (LetExp ((x,y):xs) b) env new_env =
  evalLetExp (LetExp xs b) env $ H.insert x (eval y env) new_env

evalLetExp (LetExp [] b) env new_env = eval b new_env


--helper for App
evalAppExp :: [String] -> [Exp] -> Exp -> Env -> Env -> Val
evalAppExp (x:xs) (y:ys) b env new_env =
  evalAppExp xs ys b env $ H.insert x (eval y env) new_env

evalAppExp _ _ b env new_env = eval b new_env

--- Statements
--- ----------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements
exec (SetStmt s val) penv env = ("", penv, H.insert s (eval val env) env)
--- ### Sequencing
exec (SeqStmt xs) penv env = ( (execSeqStmt xs penv env), penv, env)

--- ### If Statements
exec (IfStmt (BoolExp a) b c) penv env =
  if a
    then exec b penv env
    else exec c penv env
exec (IfStmt _ _ _) penv env = ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements


--exec helper
execSeqStmt :: [Stmt] -> PEnv -> Env -> String
execSeqStmt (PrintStmt x:xs) penv env = show (eval x env) ++ execSeqStmt xs penv env
execSeqStmt _ _ _ = []

