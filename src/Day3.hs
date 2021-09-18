{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day3
  ( solutionDay3a
  , solutionDay3b
  ) where

import           Common
import           Control.Lens
import           Control.Lens.Regex.Text
import           Data.List               as L
import qualified Data.Text               as T
import           Text.RawString.QQ
import           Text.Read

-------------------------------------------------------
-- Types
-------------------------------------------------------
data Coord =
  Coord (Int, Int)
  deriving (Show, Read, Eq)

type TreeMap = [String]

-------------------------------
-- Part 1
-------------------------------
multiplesof3 = map (3 *) [0 ..]

coordList ymax = map Coord $ zip multiplesof3 [0 .. ymax]

-- | Test if a Coordinate contains a tree
hasTree :: TreeMap -> Coord -> Bool
hasTree treemap (Coord (x, y))
  | treemap !! y !! x == '#' = True
  | otherwise = False

-- | Check how many trees are on the whole Coordinate list
howManyTrees :: TreeMap -> [Coord] -> Int
howManyTrees treemap = length . filter (hasTree treemap)

-- | Solution to Day3 Part1
solutionDay3a :: IO ()
solutionDay3a = do
  inputlines <- loadAndSplitLines "input3.txt"
  let ymax = length inputlines - 1
  let repeatedPattern = map cycle inputlines
  let coord2check = coordList ymax
  let numtrees = howManyTrees repeatedPattern coord2check
  print numtrees

---------------------------------------------------
--Part 2
---------------------------------------------------
xs1 = [0 ..]

xs2 = multiplesof3

xs3 = map (5 *) [0 ..]

xs4 = map (7 *) [0 ..]

xslist = [xs1, xs2, xs3, xs4]

coordlists = coordlist5 : map coordlist xslist

coordlist xs ymax = map Coord $ zip xs [0 .. ymax]

coordlist5 ymax = map Coord $ zip [0 ..] $ map (2 *) [0 .. ymax `div` 2]

-- | Solution to Day3 Part2
solutionDay3b :: IO ()
solutionDay3b = do
  inputlines <- loadAndSplitLines "input3.txt"
  let ymax = length inputlines - 1
  let repeatedPattern = map cycle inputlines
  let coords = map ($ymax) coordlists
  let numtrees = map (howManyTrees repeatedPattern) coords
  let sol = product numtrees
  print sol

---------------------------------------------------
--unrelated stuff
--------------------------------------------------
-- | Just a test, please ignore
txt = "raindrops on roses and whiskers on kittens"

-- | Just a test, please ignore
hastxt = has ([regex|whisk|]) txt
