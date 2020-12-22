module Day22 
(solutionDay22a,
example22b,
solutionDay22b
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

------------
--Part2
------------

recursiveCombat :: [Int]->[Int]-> [[Int]] -> Int
recursiveCombat [] list _ = sum $ zipWith (*) [1..] $ reverse list
recursiveCombat list [] _ = sum $ zipWith (*) [1..] $ reverse list
recursiveCombat p1 p2 prev
    |p1++p2 `elem` prev = sum $ zipWith (*) [1..] $ reverse p1
    |head p1 < length p1 && head p2 < length p2 = if subgame (take (head p1) (tail p1)) (take (head p2)(tail p2)) [] then recursiveCombat (tail p1 ++ [head p1, head p2]) (tail p2) ((p1++p2):prev) else recursiveCombat (tail p1) (tail p2 ++ [head p2, head p1])((p1++p2):prev)
    |head p1 > head p2 = recursiveCombat (tail p1 ++ [head p1, head p2]) (tail p2) ((p1++p2):prev)
    |otherwise         = recursiveCombat (tail p1) (tail p2 ++ [head p2, head p1]) ((p1++p2):prev)

            
            
subgame [] list _ = False
subgame list [] _ = True
subgame p3 p4 prev'
    |p3++p4 `elem` prev' = True
    |head p3 > head p4 = subgame (tail p3 ++ [head p3, head p4]) (tail p4) ((p3++p4):prev')
    |otherwise         = subgame (tail p3) (tail p4 ++ [head p4, head p3]) ((p3++p4):prev')

example22b :: IO ()
example22b = part2 "example22.txt"

solutionDay22b :: IO  ()
solutionDay22b = part2 "input22.txt"

part2 :: String -> IO ()
part2 filename = do
    players <- splitOnBlankLine filename
    let player1Deck = map read $ drop 1 $ lines $ head players
    let player2Deck = map read $ drop 1 $ lines $ last players
    let winningScore = recursiveCombat player1Deck player2Deck []
    print winningScore
    