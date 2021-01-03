module Day13 
(solution13a,
solution13b

)
where

import Common
import Data.List
import Data.Ord
import Data.List.Split
import Text.Read
import Data.Maybe
import Math.NumberTheory.Moduli.Chinese


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



----------
--Part2
----------

-- | Testing on the exmaples
test13b' = earliest [ (17,0), (13,2) ,(19,3)]

test13b'' = earliest [(67,0),(7,1),(59,2),(61,3)]

testlcm   = lcmpair [ (17,0), (13,2) ,(19,3)]

testallNi = allNi [ (17,0), (13,2) ,(19,3)]

lcmpair = product . map fst

lcmpair' = product . map snd

allNi list = map (\x ->  lcmpair list `div` fst x ) list

remainder :: Integer -> (Integer, Integer) -> Bool
remainder x (i,rem) = (x + rem) `mod` i == 0

earliest list = head [x|x<-[0..], all (remainder x) list]

toPairs list = zip ( splitOn ","  list) [0..]

pairToNumbers :: [(String,a)] -> [(Integer,a)] 
pairToNumbers pair = map fromJust $ filter isJust $ map readMaybePair pair
    where readMaybePair (a,b) = case readMaybe a of 
            Nothing -> Nothing
            Just x -> Just (x,b)



pairToNumbers' :: [(String, a)] -> [(a,Integer)] 
pairToNumbers' pair = map fromJust $ filter isJust $ map readMaybePair pair
    where readMaybePair (a,b) = case readMaybe a of 
            Nothing -> Nothing
            Just x -> Just (b,x)

-- | This function is too inefficient on the input - don't use           
solution13b' :: IO ()
solution13b' = do
    lines <-loadAndSplitLines "input13.txt"
    let input = pairToNumbers $ toPairs $ lines !! 1 
    let result = earliest input
    print result

-- | This is much more efficient using a number theory library
solution13b :: IO ()
solution13b = do
    lines <-loadAndSplitLines "input13.txt"
    let input = pairToNumbers' $ toPairs $ lines !! 1 
    let lcm' = lcmpair' input
    let rem = chineseRemainder input
    let result = fmap (lcm' - ) rem
    print result