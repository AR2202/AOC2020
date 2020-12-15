module Day15 
(example15_1,
example15_2,
addNext,
number2020,
solutionDay15a

)
where

example15_1 :: [Int]
example15_1 = [0,3,6]
example15_2 :: [Int]
example15_2 = [1,3,2]
input15 = [2,15,0,9,1,20]

addNext list = list ++ [next]
    where next
           |last list `elem` init list = 1 + (length $ takeWhile (/= last list) $ reverse $init list)
           |otherwise                  = 0

number2020 initialNums = last $ (iterate addNext initialNums)!!(2020-length initialNums)

solutionDay15a = number2020 input15