{-# LANGUAGE FlexibleContexts #-}

module Day10
  ( example10_1
  , example10_2_b
  , example10_2
  , solutionDay10a
  , solutionDay10b
  ) where

import           Common
import           Data.Function (fix)
import           Data.List     (sort)
import qualified Data.Vector   as V

jolts :: String -> IO ()
jolts filename = do
  lines <- readLines filename
  let sorted = sort lines
  let previous = 0 : sorted
  let diffToPrevious = zipWith (-) sorted previous
  let ones = (length . filter (== 1)) diffToPrevious
  let threes = 1 + (length . filter (== 3)) diffToPrevious
  print (threes * ones)

example10_1 :: IO ()
example10_1 = jolts "example10_1.txt"

example10_2 :: IO ()
example10_2 = jolts "example10_2.txt"

solutionDay10a :: IO ()
solutionDay10a = jolts "input10.txt"

----------------------------------
--Part 2
----------------------------------
connected'' _ recFn 0 = 1
connected'' list recFn x =
  sum $ fmap recFn $ takeWhile (\n -> x - n <= 3) $ dropWhile (>= x) $ list

connected list = fix $ connected'' list

connectedFaster list x = fn (\n -> v V.! n)
  where
    v = V.generate x $fn (\n -> v V.! n)
    fn = connected'' list

example10_1_b :: IO ()
example10_1_b = possibilities "example10_1.txt"

example10_2_b :: IO ()
example10_2_b = possibilities "example10_2.txt"

solutionDay10b :: IO ()
solutionDay10b = possibilities "input10.txt"

possibilities :: String -> IO ()
possibilities filename = do
  lines <- readLines filename
  let sorted = sort lines
  let firstAdded = 0 : sorted
  let reversed = reverse firstAdded
  let numPossibilities =
        connectedFaster reversed (head reversed + 1) (head reversed)
  print numPossibilities
