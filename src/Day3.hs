{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day3
(
solutionDay3a)
where

import Control.Lens
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T
import Text.Read
import Common
import Data.List as L

-- Types
-------------------------------------------------------
data Coord = Coord (Int,Int) deriving (Show, Read, Eq)
type TreeMap = [String]


multiplesof3 = map (3*) [0..]
coordList ymax = map Coord $ zip multiplesof3 [0..ymax]

-- | Test if a Coordinate contains a tree
hasTree ::  TreeMap -> Coord -> Bool
hasTree  treemap (Coord (x,y))
    |treemap !!y !!x == '#' = True
    |otherwise              = False

-- | Check how many trees are on the whole Coordinate list
howManyTrees ::  TreeMap -> [Coord] -> Int
howManyTrees treemap  = length . filter (hasTree treemap) 

-- | Solution to Day3 Part1
solutionDay3a :: IO ()
solutionDay3a = do
    inputlines <- loadAndSplitLines "input3.txt"
    let ymax = length inputlines -1
    let repeatedPattern = map cycle inputlines
    let coord2check = coordList ymax
    let numtrees = howManyTrees repeatedPattern coord2check
    print numtrees

-- | Just a test, please ignore
txt = "raindrops on roses and whiskers on kittens"
-- | Just a test, please ignore
hastxt = has ([regex|whisk|]) txt
    