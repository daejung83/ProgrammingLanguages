module Lib
    ( someFunc
    , Exp(..)
    , display
    , parse
    ) where

import Text.Regex.TDFA

data Exp = PlusExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LetExp String Exp Exp
    deriving (Show,Eq)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

display :: Exp -> String
display (VarExp v) = v
display (PlusExp e1 e2) = "+ " ++ display e1 ++ " " ++ display e2
display (LetExp v e1 e2) = "let " ++ v ++ " = " ++ display e1 ++ " in " ++ display e2 ++ " end"
display (IntExp i) = show i

isInt :: String -> Bool
isInt i = i =~ "[0-9]+"

isSymbol :: String -> String -> Bool
isSymbol s v = s == v

parseSymbol s (x:xs) =
  if s == x
     then (s,xs)
     else error $ "Parse error, expected " ++ s ++ " but got " ++ x ++ "."

-- Grammar
--
-- E -> + E E
--    | int
--    | var
--    | ( E )
--    | let var = E in E end

parse xx = parseE (words xx)

parseE ("+":xs) =
  let (e1,r1) = parseE xs
      (e2,r2) = parseE r1
   in (PlusExp e1 e2, r2)

--parseE ("(":xs) = parseE xs
    --let (e1, r1) = parseE (xs)
    --    in parseE r1

parseE ("let":xs) =
    let (e1,r1) = parseE xs
        (_, rr1) = parseSymbol "=" r1
        (e2,r2) = parseE rr1
        (_, rr2) = parseSymbol "in" r2
        (e3,r3) = parseE rr2
        (_, rr3) = parseSymbol "end" r3
        in ((LetExp (display e1) e2 e3), rr3)

--parseE ("end":xs) = parseE xs
--parseE ("in":xs) = parseE xs

parseE (x:xs) | isInt x =
                (IntExp (read x), xs)
              | isSymbol "(" x = 
                    let (e1, r1) = parseE xs
                        (e2, rr1) = (parseSymbol ")" r1)
                    in (e1, rr1)
              | otherwise =
                (VarExp x, xs)  




