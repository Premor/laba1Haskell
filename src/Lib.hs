module Lib
    ( 
        someFunc,
        anotherFunc,
        laba1_1,
        laba1_2
    ) where

someFunc :: String -> Maybe Int 
anotherFunc :: Int -> Bool
laba1_1 :: Int -> Int -> Int
laba1_2 :: [Int] -> [Int]


laba1_1 x y 
    | y == 0 = 1    
    | y == 1 = x
    | otherwise = laba1_1 (x*2) (y-1)

laba1_2 x = [i * i | i <- x]





someFunc x =
        let i =read x::Int in
        if i > 10 
            then Just i
            else Nothing 

anotherFunc x 
        | x == 0 = True
        | otherwise = False