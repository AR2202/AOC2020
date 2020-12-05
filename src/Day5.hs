module Day5
(
solutionDay5a,
solutionDay5b)
where

import Common
import Data.List


data FrontBack = F|B deriving (Show, Read,Eq)
data LeftRight = L|R deriving (Show, Read,Eq)
wholeRange = [0..127]
wholeColRange = [0..7]
example1d5 = "FBFBBFF"
example1d5' = "RLR"
chr2FB 'F' = F
chr2FB 'B' = B
chr2LR 'L' = L
chr2LR 'R' = R

example1d5AsFB :: [FrontBack]
example1d5AsFB = map chr2FB example1d5

example1d5AsLR :: [LeftRight]
example1d5AsLR = map chr2LR example1d5'

newRange oldRange F = take (length oldRange `div`2) oldRange
newRange oldRange B = drop (length oldRange `div`2) oldRange
findRow frontbacklist = head $ foldl newRange wholeRange frontbacklist

newRange' oldRange L = take (length oldRange `div`2) oldRange
newRange' oldRange R = drop (length oldRange `div`2) oldRange
findCol lrlist = head $ foldl newRange' wholeColRange lrlist

idRC row col = 8*row + col

splitRowsCols  = splitAt 7

toFB (row,col) = map chr2FB row
toLR (row,col) = map chr2LR col

findId passinf = idRC (findRow $toFB passinf) (findCol$toLR passinf)
solutionDay5a = do
    boardingpasses <- loadAndSplitLines "input5.txt"
    let rowscols = map splitRowsCols boardingpasses
    let ids = map findId rowscols
    let maxid = maximum ids
    print maxid


-------------
--Part 2
-------------

allIds = [0..8*128-1]

missingWithExistingNeighbours idlist x = x `notElem` idlist && x+1 `elem` idlist && x-1 `elem` idlist

solutionDay5b = do
    boardingpasses <- loadAndSplitLines "input5.txt"
    let rowscols = map splitRowsCols boardingpasses
    let ids = map findId rowscols
    let missing = filter (missingWithExistingNeighbours ids) allIds
    print $ head missing
