module Day23
(example23a,
newlist,
example23a100moves,
solutionDay23a

)
where
import qualified Data.IntMap.Strict as IM
import Data.List


example23 = ["3","8","9","1","2","5","4","6","7"]
input23 = ["3","1","8","9","4","6","5","7","2"]

toDigitList:: [String] -> [Int]
toDigitList = map read

destination list = determineDestination list (head list)
    where determineDestination list x 
            |x == 1 = determineDestination list 10
            |(x - 1) `notElem` (removed list) = x - 1
            
            |otherwise = determineDestination list (x - 1)
          

removed list = take 3 $ tail list

newlist list = takeWhile (/= destination list) (tail list') ++ [destination list] ++ (removed list)++ drop 1 ( dropWhile (/= destination list) (tail list')) ++ [head list]
    where list' = list \\(removed list)
            

answer list = drop 1 $dropWhile(/=1) list++ takeWhile (/=1)list

example23a = answer $ (iterate newlist $ toDigitList example23) !! 10

example23a100moves = answer $ (iterate newlist $ toDigitList example23) !! 100

solutionDay23a = answer $ (iterate newlist $ toDigitList input23) !! 100
