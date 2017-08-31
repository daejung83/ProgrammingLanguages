{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

lowerList :: Val -> EvalState [Val]
lowerList (List xx) = return xx
lowerList v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
  p [] = return $ Number c
  p [x] = Number . f c <$> lowerInt x
  p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

-- TODO
liftIntBinOp :: (Int -> Int -> Int) -> Val
liftIntBinOp f = PrimFunc p where
  -- You should replace the following line with your own implementation
  p [] = throwError $ UnexpectedArgs []
  p [x] = throwError $ UnexpectedArgs [x]
  p xx = Number . foldl1 f <$> mapM lowerInt xx

-- TODO
liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp f = PrimFunc p where
  -- You should replace the following line with your own implementation
  p [Number x] = return $ Number $ f x
  p v = throwError $ UnexpectedArgs v

-- liftBoolUnaryOp :: (Bool -> Bool) -> Val
-- liftBoolUnaryOp f = PrimFunc p where
--   p [Boolean x] = return $ Boolean $ f x
--   p [Number x] = case x of
--                     0 -> return $ Boolean True
--                     _ -> return $ Boolean False
--   p v = throwError $ UnexpectedArgs v

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
  p [Boolean x] = return $ Boolean $ f x
  p [_] = return $ Boolean False
  p v = throwError $ UnexpectedArgs v

-- TODO
liftCompOp :: (Int -> Int -> Bool) -> Val
liftCompOp f =  PrimFunc p where
  -- You should replace the following line with your own implementation
  p [] = return $ Boolean True
  p [x] = return $ Boolean True
  p ((Number x):(Number y):xx) = if f x y
                      then p ((Number y):xx)
                      else return $ Boolean False
  p v = throwError $ UnexpectedArgs v


--- ### Primtive operations

-- Primitive function `car`
-- TODO
car :: [Val] -> EvalState Val
car [List (x:xs)] = return x
car [DottedList(x:xs) _] = return $ x
car v = throwError $ UnexpectedArgs v

-- Primitive function `cdr`
-- TODO
cdr :: [Val] -> EvalState Val
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (x:xs) v] = return $ DottedList xs v
cdr v = throwError $ UnexpectedArgs v

-- Primitive function `cons`
-- TODO
cons :: [Val] -> EvalState Val
cons [x,y] = return $ DottedList [x] y
cons v = throwError $ UnexpectedArgs v

list :: [Val] -> EvalState Val
list x = return $ List x

-- Primitive function `append`
append :: [Val] -> EvalState Val
append [] = return $ List []
append [x] = return x
append vv = foldlM append' (List []) (map flattenList vv) where
  append' (List []) x = return x
  append' (List xs) (List ys) = return $ List (xs ++ ys)
  append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
  append' _ acc = throwError $ TypeError acc

-- Primitive function `apply`
-- It applies a function to a list of parameters
-- TODO
-- Examples:
--   (apply + '(1 2 3))  => 6
--   (apply car '((1 2 3)))  => 1
applyPrim :: [Val] -> EvalState Val
applyPrim [f, (List xs)] = apply f xs

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
-- TODO
-- Examples:
--   (eval '(+ 1 2 3))  => 6
evalPrim :: [Val] -> EvalState Val
evalPrim [xx] = eval xx

-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
-- TODO
-- Examples:
--   (= 1 1) => #t
--   (= #f #t) => #f
--   (= #f #f) => #t
--   (= 'a 10) => Type error
--   (= 'a 'b) => Type error
equalSign :: [Val] -> EvalState Val
equalSign [] = return $ Boolean True
equalSign ((Number x):(Number y):xs) = aux1 ((Number x):(Number y):xs)
    where aux1 ((Number x):(Number y):xs) | x /= y = return $ Boolean False
                                          | otherwise = aux1 ((Number y):xs)
          aux1 [Number x] = return $ Boolean True
          aux1 [] = return $ Boolean True
          aux1 ((Number x):v:xs) = throwError $ TypeError v
          aux1 (v:_) = throwError $ TypeError v
equalSign ((Boolean x):(Boolean y):xs) = aux2 ((Boolean x):(Boolean y):xs)
    where aux2 ((Boolean x):(Boolean y):xs) | x /= y = return $ Boolean False
                                            | otherwise = aux2 ((Boolean y):xs)
          aux2 [Boolean x] = return $ Boolean True
          aux2 [] = return $ Boolean True
          aux2 ((Boolean x):v:xs) = throwError $ TypeError v
          aux2 (v:_) = throwError $ TypeError v
equalSign (v:_) = throwError $ TypeError v

-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
-- TODO
-- Examples:
--   (eq? 1 1) => #t
--   (eq? #f #t) => #f
--   (eq? #f #f) => #t
--   (eq? 'a 10) => #f
--   (eq? 'a 'a) => #t
eq :: [Val] -> EvalState Val
eq [] = return $ Boolean True
eq ((Number x):(Number y):xs) = aux ((Number x):(Number y):xs)
    where aux ((Number x):(Number y):xs) | x /= y = return $ Boolean False
                                         | otherwise = aux ((Number y):xs)
          aux [Number x] = return $ Boolean True
          aux [] = return $ Boolean True
          aux (v:_) = return $ Boolean False
eq ((Boolean x):(Boolean y):xs) = aux ((Boolean x):(Boolean y):xs)
    where aux ((Boolean x):(Boolean y):xs) | x /= y = return $ Boolean False
                                           | otherwise = aux ((Boolean y):xs)
          aux [Boolean x] = return $ Boolean True
          aux [] = return $ Boolean True
          aux (v:_) = return $ Boolean False
eq ((Symbol x):(Symbol y):xs) = aux ((Symbol x):(Symbol y):xs)
    where aux ((Symbol x):(Symbol y):xs) | x /= y = return $ Boolean False
                                           | otherwise = aux ((Symbol y):xs)
          aux [Symbol x] = return $ Boolean True
          aux [] = return $ Boolean True
          aux (v:_) = return $ Boolean False
eq (v:_) = return $ Boolean False

-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
-- TODO
isList :: [Val] -> EvalState Val
isList [v] =
  return . Boolean $ case flattenList v of
    List _ -> True
    _ -> False
isList vv = throwError $ UnexpectedArgs vv

-- Primitive function `symbol?` predicate
-- TODO
isSymbol :: [Val] -> EvalState Val
isSymbol [Symbol _] = return $ Boolean True
isSymbol [_] = return $ Boolean False
isSymbol vv = throwError $ UnexpectedArgs vv

-- Primitive function `pair?` predicate
-- Any `List` or `DottedList` is a pair
-- TODO
isPair :: [Val] -> EvalState Val
isPair [List x] = return $ Boolean $ case x of
                                        (x:xs) -> True
                                        _ -> False
isPair [DottedList _ _] = return $ Boolean True
isPair [_] = return $ Boolean False
isPair vv = throwError $ UnexpectedArgs vv

-- Primitive function `number?` predicate
-- TODO
isNumber :: [Val] -> EvalState Val
isNumber [Number _] = return $ Boolean True
isNumber [_] = return $ Boolean False
isNumber vv = throwError $ UnexpectedArgs vv

-- Primitive function `boolean?` predicate
-- TODO
isBoolean :: [Val] -> EvalState Val
isBoolean [Boolean _] = return $ Boolean True
isBoolean [_] = return $ Boolean False
isBoolean vv = throwError $ UnexpectedArgs vv

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
-- TODO
isNull :: [Val] -> EvalState Val
isNull [] = throwError $ UnexpectedArgs []
isNull [Void] = return $ Boolean True
isNull [List x] = return $ Boolean $ case x of
                                        (x:xs) -> False
                                        _ -> True
isNull [_] = return $ Boolean False
isNull vv = throwError $ UnexpectedArgs vv

--- ### Runtime

runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("and", liftBoolVargOp and)
                     , ("or", liftBoolVargOp or)
                     , ("cons", PrimFunc cons)
                     , ("append", PrimFunc append)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("list?", PrimFunc isList)
                     -- TODO: Insert more runtime bindings here
                     , ("*", liftIntVargOp (*) 1)
                     , ("/", liftIntVargOp (div) 1)
                     , (">", liftCompOp (>))
                     , ("<", liftCompOp (<))
                     , (">=", liftCompOp (>=))
                     , ("<=", liftCompOp (<=))
                     , ("car", PrimFunc car)
                     , ("cdr", PrimFunc cdr)
                     , ("list", PrimFunc list)
                     , ("not", liftBoolUnaryOp not)
                     , ("=", PrimFunc equalSign)
                     , ("eq?", PrimFunc eq)
                     , ("modulo", liftIntBinOp mod)
                     , ("abs", liftIntUnaryOp abs)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("list?", PrimFunc isList)
                     , ("pair?", PrimFunc isPair)
                     , ("null?", PrimFunc isNull)
                     , ("number?", PrimFunc isNumber)
                     , ("boolean?", PrimFunc isBoolean)
                     , ("apply", PrimFunc applyPrim)
                     , ("eval", PrimFunc evalPrim)
                     ]
