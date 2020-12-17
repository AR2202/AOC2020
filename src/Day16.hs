{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-#LANGUAGE BangPatterns#-}
module Day16 (
example16a,
example16b,
solutionDay16a,
solutionDay16b,
allInRange,
findFields,
intersectSecond,
singles
)
where

import Common
import Control.Lens
import Data.List (nub,(\\))
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T


exampletext = "departure location: 48-793 or 800-971"
exampletext2 = "7,3,47"
----------------------------
--Part 1
----------------------------

matchDigits :: String -> [[T.Text]]
matchDigits txt = T.pack txt ^.. [regex|(\d+)-(\d+)|] . groups

readToInt :: T.Text -> Int
readToInt txt = read $ T.unpack txt

readAllToInt :: [[T.Text]] -> [[Int]]
readAllToInt = (map . map) readToInt

readLineToInt :: String -> [[Int]]
readLineToInt = readAllToInt . matchDigits

makeRange :: [Int]->[Int]
makeRange list = [x..y]
    where x = head list
          y = list !! 1

lineToIntList :: String -> [Int]
lineToIntList = nub . concatMap makeRange . readLineToInt

linesToIntList :: [String] -> [Int]
linesToIntList = nub . concatMap lineToIntList

matchTickets :: String -> [T.Text]
matchTickets txt = T.pack txt ^.. [regex|(\d+)|] . match

readTickets :: String -> [Int]
readTickets = map (read . T.unpack) . matchTickets

invalidNums numbers validRange = filter (`notElem` validRange) numbers


part1 filename = do
    parts <- splitOnBlankLine filename
    let ranges = linesToIntList $ lines $ head parts
    let tickets = map readTickets $ drop 1 $ lines $ parts !! 2
    let ticketvalues = concat tickets
    let invalids = invalidNums ticketvalues ranges
    let result = sum invalids
    print result

example16a = part1 "example16.txt"

solutionDay16a = part1 "input16.txt"

-------------------------
--Part2
-------------------------

part2 filename = do
    parts <- splitOnBlankLine filename
    let !range = linesToIntList $ lines $ head parts
    let !ranges = map lineToIntList $ lines $ head parts
    let !tickets = map readTickets $ drop 1 $ lines $ parts !! 2
    let !yourticket = head $map readTickets $ drop 1 $ lines $ parts !! 1
    let !validTickets = filter (isValidTicket range) tickets
    let !validFields = map (\r-> filter (allInRange r validTickets) [0..length (head validTickets)-1]) ranges
    let !rowsFields = zip [1..] validFields
    let !fields = findFields rowsFields []
    let !departurefields = map (head .snd) $filter (\x-> fst x `elem`[1..6]) fields
    let !departureYourTicket = map (yourticket!!) departurefields
    print $ product departureYourTicket
    

example16b = part2 "example16.txt"    

solutionDay16b = part2 "input16.txt"

isValidTicket range ticket = null (invalidNums ticket range) 

allInRange range ticketlist x = all (\list -> list!!x `elem` range) ticketlist

singles = filter (\x->(length ( snd x)) == 1) 

intersectSecond list2 list1 = (fst list1, (snd list1) \\ list2)

findFields :: [(Int,[Int])]->[(Int,[Int])] -> [(Int,[Int])]
findFields [] !identified = identified
findFields !unidentified !identified = findFields  newunidentified (newidentified ++ identified)
    where newidentified = singles unidentified
          newunidentified = map (intersectSecond (nub ( concatMap snd newidentified))) $ unidentified \\ newidentified
