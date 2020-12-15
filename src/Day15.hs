{-#LANGUAGE BangPatterns#-}
module Day15 
(example15_1,
example15_2,
addNext,
number2020,
solutionDay15a,
solutionDay15b,
solutionDay15a',
examplemap15_1,
insertNext,
insertAll

)
where
import Data.IntMap.Strict as IM
import Data.Maybe
import Data.List as L

------------------------------
--examples
------------------------------
example15_1 :: [Int]
example15_1 = [0,3,6]
example15_2 :: [Int]
example15_2 = [1,3,2]
input15 = [2,15,0,9,1,20]
-------------------------------
--Part1
-------------------------------

addNext list = list ++ [next]
    where next
           |last list `elem` init list = 1 + (length $ takeWhile (/= last list) $ reverse $init list)
           |otherwise                  = 0

number2020 initialNums = last $ (iterate addNext initialNums)!!(2020-length initialNums)

solutionDay15a = number2020 input15

---------------------------
--Part2
---------------------------
examplemap15_1 :: IntMap Int
examplemap15_1 = IM.fromList $ zip  example15_1 [1..]

insertNext !intmap !prev ind = (IM.insert prev ind intmap, next)
    where next =  case (IM.lookup prev intmap) of
              Nothing -> 0
              Just x  -> ind - x

insertAll !startlist n = L.foldl' (uncurry insertNext)  (startmap,last startlist)[length startlist..n-1]
    where !startmap = IM.fromList $ zip (init startlist) [1..]

solutionDay15a' = snd $ insertAll input15 2020
solutionDay15b = snd $ insertAll input15 30000000