module Day10 
(example10_1,
example10_2,
solutionDay10a)
where

import Common
import Data.List(sort)

jolts :: String -> IO ()
jolts filename = do
    lines <- readLines filename
    let sorted = sort lines
    let previous = 0:sorted
    let diffToPrevious = zipWith (-) sorted previous
    let ones = (length . filter (==1)) diffToPrevious
    let threes = 1 + (length . filter (==3)) diffToPrevious
    print (threes * ones)

example10_1 :: IO ()
example10_1 = jolts "example10_1.txt"

example10_2 :: IO ()
example10_2 = jolts "example10_2.txt"
solutionDay10a :: IO ()
solutionDay10a = jolts "input10.txt"