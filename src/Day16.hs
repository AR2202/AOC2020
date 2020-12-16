{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day16 (
example16a,
solutionDay16a
)
where

import Common
import Control.Lens
import Data.List (nub)
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