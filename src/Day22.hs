module Day22 
(solutionDay22a
)
where

import Common

solutionDay22a :: IO ()
solutionDay22a = part1 "input22.txt"

example22a :: IO ()
example22a = part1 "example21.txt"

part1 :: String -> IO ()
part1 filename = do
    players <- splitOnBlankLine filename
    let player1Deck = map read $ drop 1 $ lines $ head players
    let player2Deck = map read $ drop 1 $ lines $ last players
    let winningScore = cardGame player1Deck player2Deck
    print winningScore
    

cardGame :: [Int]->[Int]-> Int
cardGame [] list = sum $ zipWith (*) [1..] $ reverse list
cardGame list [] = sum $ zipWith (*) [1..] $ reverse list
cardGame p1 p2  
    |head p1 > head p2 = cardGame (tail p1 ++ [head p1, head p2]) (tail p2)
    |otherwise         = cardGame (tail p1) (tail p2 ++ [head p2, head p1])