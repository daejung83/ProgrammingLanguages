module Lib
    (
        --exports listed here
        alwaysFalseFunc,
        alwaysListOfSumTen,
        Fruit(Apple, Watermelon, Banana)
    ) where

data Fruit = Apple String | Watermelon | Banana Integer

data List a = Cons a (List a)
              | Nil
    deriving Show

--See how these functions are tested in test/Spec.hs

--this function always returns False
alwaysFalseFunc x = False
--this function always returns a list of ints that sum to 10
alwaysListOfSumTen x = [7,3]

-- Your scratch work goes here. Don't submit the code above!

dotproduct :: [Int] -> [Int] -> Int
dotproduct (x:xs) (y:ys) = x*y + dotproduct xs ys
dotproduct _ _ = 0

cons2list :: List a -> [a]
cons2list (Cons a x) = a : cons2list x
cons2list Nil = []

countCons :: List a -> Int
countCons (Cons _ x) = 1 + countCons x
countCons Nil = 0

aboveFive :: [Integer] -> [Integer]
aboveFive = filter (> 5)

maxk a b k = 
	if a > b
	then k a
	else k b

max3k a b c k =
	if maxk a b (\x -> 
		maxk c k (x1 ->
			k (9 + x1)))

instance Monad Foo where
	Foo x >>= f = f x
	Bar >>= f = Bar
	return x = Foo x




-- REMEMBER TO SUBMIT YOUR CODE DIRECTLY ON THE EXAM PROBLEM PAGE
