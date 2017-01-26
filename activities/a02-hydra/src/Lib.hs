module Lib
    ( someFunc
    , chop
    ) where

-- Your code here!
-- Feel free to write helper functions.

-- chop :: [Int] -> [Int]

chop :: [Int] -> [Int]
--chop [] = []
--chop n = n

chop [] = []
chop n = aux n ((length n) - 1)
	where aux (x:xs) loc = do
		if xs == [] && x > 0
			then (x-1):[]	
			else if x > 0
				then (x-1):head(xs)+loc:tail(xs)
				else x:chop xs
			 
	

someFunc :: IO ()
someFunc = putStrLn "someFunc"
