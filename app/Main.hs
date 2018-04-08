module Main where

import Lib

main :: IO ()
main = do
    x <- getLine
    print $ anotherFunc <$> someFunc x

laba1_3 :: Test -> String
data Test = Gavno Int
          | Jopa Int
          | Chlen Int
          deriving (Eq,Show,Ord)

data Data = Q | W | E | R deriving(Show)

laba1_4 :: Data -> String

laba1_4 Q = "QA"

laba1_4 W = "WA"

laba1_4 E = "EA"

laba1_4 R = "RA"


laba1_3 (Gavno 1) = "Gavno"

laba1_3 (Jopa  2) = "Jopa"

laba1_3 (Chlen  3) = "Gavno"

laba1_3 (Gavno x) = "SYKA"
