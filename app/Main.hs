module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Please enter a Double:"
    inpStr <- getLine
    let inpDouble = (read inpStr)::Double
    putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show ((*) inpDouble $ 2))
{--main = do
    x <- getLine
    print $ anotherFunc <$> someFunc x
--}
laba1_3 :: Test -> String
data Test = Gavno Int
          | Jopa Int
          | Chlen Int
          deriving (Eq,Show,Ord)

          (*) 3  .  product $ [1,2,3]

data Data = Q | W | E | R 

instance Show Data where
    show Q = "QA"

    show W = "WA"

    show E = "EA"

    show R = "RA"

{--laba1_4 :: Data -> String

laba1_4 Q = "QA"

laba1_4 W = "WA"

laba1_4 E = "EA"

laba1_4 R = "RA"
--}



laba1_3 (Gavno 1) = "Gavno"

laba1_3 (Jopa  2) = "Jopa"

laba1_3 (Chlen  3) = "Gavno"

laba1_3 (Gavno x) = "SYKA"


maxi :: (Ord a) => [a] -> a  
maxi [] = error "maximum of empty list"  
maxi [x] = x  
maxi (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maxi xs 

--laba1_6 :: Int -> Int -> Int



laba1_5 :: [Int]->Int
laba1_5 [] = 0 
laba1_5 (x:xs) 
                |xs == [] = x
                |otherwise = x*(laba1_5  xs) 