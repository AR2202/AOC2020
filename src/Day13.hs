module Day13 
(solution13a

)
where

import Common
import Data.List
import Data.Ord
import Data.List.Split
import Text.Read
import Data.Maybe


toNumbers :: String -> [Int]
toNumbers list = map fromJust $ filter isJust $ map readMaybe $ splitOn ","  list


toWait :: Int -> Int -> Int
toWait timestamp id = id - (timestamp `mod` id)

minWait :: Int -> [Int] -> Int
minWait timestamp = minimumBy (comparing (toWait timestamp))

multiplied timestamp ids = minWait timestamp ids * toWait timestamp  (minWait timestamp ids)

result13 linesInput = multiplied ((read . head ) linesInput) (toNumbers $  linesInput!!1)

solution13a = do
    lines <- loadAndSplitLines "input13.txt"
    let result = result13 lines
    print result