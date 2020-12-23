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

recursiveCombat :: [Int]->[Int]-> [[Int]] -> [[Int]]-> Int
recursiveCombat [] list _ _ = sum $ zipWith (*) [1..] $ reverse list
recursiveCombat list [] _ _ = sum $ zipWith (*) [1..] $ reverse list
recursiveCombat p1 p2 prev1 prev2
    |(p1,p2) `elem` (zip prev1  prev2) = sum $ zipWith (*) [1..] $ reverse p1
    |head p1 < length p1 && head p2 < length p2 = if subgame sub1 sub2 [] [] then recursiveCombat (tail p1 ++ [head p1, head p2]) (tail p2) (p1:prev1) (p2:prev2) else recursiveCombat (tail p1) (tail p2 ++ [head p2, head p1])(p1:prev1)(p2:prev2)
    
    |head p1 > head p2 = recursiveCombat (tail p1 ++ [head p1, head p2]) (tail p2) (p1:prev1)(p2:prev2)
    |otherwise         = recursiveCombat (tail p1) (tail p2 ++ [head p2, head p1]) (p1:prev1) (p2:prev2)
        where sub1 = take (head p1) (tail p1)
              sub2 = take (head p2)(tail p2)
            
            
subgame [] list _ _ = False
subgame list [] _ _ = True
subgame p3 p4 prev1 prev2
    |(p3,p4) `elem` (zip prev1 prev2) = True
    |head p3 > head p4 = subgame (tail p3 ++ [head p3, head p4]) (tail p4) (p3:prev1) (p4:prev2)
    |otherwise         = subgame (tail p3) (tail p4 ++ [head p4, head p3]) (p3:prev1) (p4:prev2)

example22b :: IO ()
example22b = part2 "example22.txt"

solutionDay22b :: IO  ()
solutionDay22b = part2 "input22.txt"

part2 :: String -> IO ()
part2 filename = do
    players <- splitOnBlankLine filename
    let player1Deck = map read $ drop 1 $ lines $ head players
    let player2Deck = map read $ drop 1 $ lines $ last players
    let winningScore = recursiveCombat player1Deck player2Deck [][]
    print winningScore
    