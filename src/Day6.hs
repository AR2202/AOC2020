module Day6 
(solutionDay6a,
solutionDay6b
)
where

import Common
import Data.List
import Data.List.Split
import Control.Monad

--------------------------
--Part 1
---------------------------

solutionDay6a :: IO()
solutionDay6a = splitOnBlankLine "input6.txt" >>= print . sum . fmap  (length . nub  . join . lines)

------------------------------
--Part2
------------------------------

solutionDay6b :: IO ()
solutionDay6b = splitOnBlankLine "input6.txt" >>= print . sum . fmap  (length  . foldl1' intersect . lines)