module Day1
    ( example1,
      diffTo2020,
      findDiffTo2020InList,
      multiplyNumbers,
      solutionDay1a,
      findDiffInList,
      multiply3Numbers,
      solutionDay1b

    ) where

import Data.List (intersect, nub,sort)
import Common


example1 :: [Int]
example1 = [1721,979,366,299,675,1456]

--------------------
-- Part 1 of day 1
--------------------

diffTo2020 :: [Int] -> [Int]
diffTo2020 = map (2020 -)



findDiffTo2020InList :: [Int] -> [Int]
findDiffTo2020InList list = intersect list $ diffTo2020 list

multiplyNumbers :: [Int] -> Int
multiplyNumbers numInList = number * (2020 - number)
    where number = head $ sort numInList

solutionDay1a :: IO ()
solutionDay1a = do
  numberlist <- readLines "input1.txt"
  let result = multiplyNumbers $ findDiffTo2020InList numberlist
  print result

--------------------
-- Part 2 of day 1
--------------------
diffToNum :: [Int] -> Int -> [Int]
diffToNum list num = map (num -) list

findDiffInList list = list `intersect` differences
  where differences = diffTo2020 list >>= diffToNum list

multiply3Numbers :: [Int] -> Int
multiply3Numbers list = list !! 0 * list !! 1 * list !! 2

excludeDuplicates :: [Int] -> [Int]
excludeDuplicates list
  | length list > 3 = nub list
  | otherwise       = list

solutionDay1b :: IO ()
solutionDay1b = do
  numberlist <- readLines "input1.txt"
  let result = multiply3Numbers $ excludeDuplicates $ findDiffInList numberlist
  print result