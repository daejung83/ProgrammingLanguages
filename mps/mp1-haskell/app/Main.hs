--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Main where

main :: IO ()
main = return ()

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x:xs)
    | n > 0 =  x : mytake (n-1) xs
    | otherwise = []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs)
    | n > 0 = mydrop (n-1) xs
    | otherwise = x:xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev xx = revh xx []

revh :: [a] -> [a] -> [a]
revh [] a = a
revh (x:xs) a = revh xs (x:a)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] b = b
app a [] = a
app a b = apph (rev a) b

apph :: [a] -> [a] -> [a]
apph [] a = a
apph (x:xs) (ys) = apph (xs) (x:ys)


--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1):inclist(xs)

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist a = sumlisth a 0

sumlisth :: Num a => [a] -> a -> a
sumlisth [] a = a
sumlisth (x:xs) a = sumlisth xs $x + a

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip a [] = []
myzip [] b = []
myzip (x:xs) (y:ys) = (x,y): myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: Num a => [a] -> [a] -> [a]
addpairs [] b = []
addpairs a [] = []
addpairs (x:xs) (y:ys) = (x+y): addpairs xs ys

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = [1, 1 ..]

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a [] = [a]
add a (x:xs)
        | a < x = a:x:xs
        | a > x = x:add a xs
        | a == x = x:xs
--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union a [] = a
union [] b = b
union xs ys = foldr add xs ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] a = []
intersect b [] = []
intersect (x:xs) (y:ys)
    | x > y = intersect (x:xs) ys
    | x == y = x : intersect xs ys
    | otherwise = intersect xs (y:ys)
--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (map (x:) (powerset xs)) (powerset xs)

--powerhelp xs ys a = foldr (\x acc -> add(x a) :acc) a xs

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' xx = map (\x -> (x + 1)) xx

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: Num a => [a] -> a
sumlist' xx = foldr (+) 0 xx

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons(xs))

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x y) = x: cons2list y

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x) = x
eval (PlusExp (xs)) = foldr (\x acc -> eval(x) + acc) 0 xs
eval (MultExp (xs)) = foldr (\x acc -> eval(x) * acc) 1 xs
--eval _ = 0
--eval (IntExp a) = a
--eval (PlusExp a) = evaladd a 0
--eval (MultExp a) = evalmul a 1

--evaladd :: [Exp] -> Integer -> Integer
--evaladd [] a = a
--evaladd (IntExp x:xs) a = a + evaladd xs x
--evaladd x a = a + evalmul x 1

--evalmul :: [Exp] -> Integer -> Integer
--evalmul [] a = a
--evalmul (IntExp x:xs) a = a * evalmul xs x
--evalmul x a = a * evaladd x 0

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' [] = Nil
list2cons' (xs) = foldr (\x acc -> (Cons x acc)) Nil xs

--- ### BinTree

-- BinTree
data BinTree a = Leaf
                | Node a (BinTree a) (BinTree a)
                deriving (Show)


--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree (Leaf) = 0
sumTree (Node a t1 t2) = a + sumTree(t1) + sumTree(t2) 

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer
            | BoolVal Bool
            | StrVal String
            | ExnVal String
            deriving (Show)

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal a) (IntVal b) = IntVal(f a b)
liftIntOp f _ _ = ExnVal "not an IntVal!"
