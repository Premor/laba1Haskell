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


--(*) 3  .  product $ [1,2,3]



{--laba1_4 :: Data -> String

laba1_4 Q = "QA"

laba1_4 W = "WA"

laba1_4 E = "EA"

laba1_4 R = "RA"
--}





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


laba2_1:: IO String

laba2_1 = do
    handle <- readFile "./test.txt"
    let ln = words  handle
        (xa:ya:za:xsa) = ln
        anoWord (x:xs) l 
            | l > 1 = x:anoWord xs (l-1)
            | otherwise = [x]
        eith i = anoWord i (round ((fromIntegral (length i)) * 0.8))
        res (x:xs) 
            |xs == [] = [eith x]
            |otherwise = (eith x):res xs 
    print ln
    print (round ((fromIntegral (length ya)) * 0.8))
    print ((joinList . res $ ln) " ")
    return ((joinList . res $ ln) " ")

--eith i = anoWord i . round . (*) 0.8 . fromIntegral . length $ i 
joinList::[String] -> String -> String
joinList (x:xs) s 
    |xs == [] = x
    |otherwise = x++s++(joinList xs s)

data Times = Utro | Den' | Vecher | Noch' deriving (Show,Read)
    
class Test m where
    test::Num a =>m a->a

data Dela a = Dela Times a deriving (Show,Read)

instance Test Dela where
    test (Dela Utro a) = a + 2
    test (Dela Den' a) = a + 3
    test (Dela Vecher a) = a + 4
    test (Dela Noch' a) = a + 5



{--laba2_2::Times -> Dela -> IO ()
laba2_2 x y = do
    print x
    print y--}
    
laba2_3:: (Eq a,Num a)=>[a] -> a -> [Maybe a]
laba2_3 (x:xs) d
    | xs == [] = if x == d 
        then [Nothing]
        else [Just x]
    | x == d = Nothing:laba2_3 xs d
    | otherwise = Just x:laba2_3 xs d

myMap:: (a->a)->[Maybe a]->[Maybe a]
myMap f (Just a:[]) = [Just (f a)]
myMap f (Nothing:[]) = [Nothing]
myMap f (Nothing:xs) = Nothing:myMap f xs
myMap f (Just a:xs) = Just (f a):myMap f xs
