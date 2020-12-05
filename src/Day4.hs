{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day4
(solutionDay4b,
solutionDay4b_,
solutionDay4a,
readPassportField,
byrValid)
where

import Common
import Control.Lens
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T
import Text.Read
import Data.Maybe

passportFields txt =   txt ^.. [regex|\b(byr|iyr|eyr|hgt|hcl|ecl|pid):|] . match

hasAllRequiredFields = (==7) . length . passportFields 

toTextAndCheckRequiredFields = hasAllRequiredFields . T.pack


solutionDay4a :: IO()
solutionDay4a = (splitOnBlankLine "input4.txt")  >>= print . length . filter toTextAndCheckRequiredFields 

-------------------
--Part 2
-------------------

data PassportField = BYR | IYR | EYR | HGT | HCL | ECL |PID |NONE deriving (Show,Read,Eq)

passportFieldsWithData txt =   txt ^.. [regex|\b(byr|iyr|eyr|hgt|hcl|ecl|pid):#?\w*\b|] . match
splitFieldnameFromData txt =  listToTuple $ fromMaybe [] (txt ^?  [regex|(\w+):(#?\w+\b)|] . groups)

listToTuple :: [T.Text] -> (PassportField,T.Text)
listToTuple []=(NONE,"")
listToTuple (x:[]) = ((readPassportField . T.toUpper )x,"")
listToTuple (x:y:xs) = ((readPassportField . T.toUpper) x,y)

solutionDay4b_ :: Int -> IO()
solutionDay4b_ x = do
    passportinfo <- splitOnBlankLine  "input4.txt"
    let first = passportinfo !! x
    
    print first
    
    print $ fieldNames $ filter (uncurry dataIsValid) $ map splitFieldnameFromData $ passportFieldsWithData $ T.pack first
    print $ filter (uncurry dataIsValid) $ map splitFieldnameFromData $ passportFieldsWithData $ T.pack first
    print $ hasAllFieldnames first
    print $ hasAllValidFields first
    print $ numValidFields first
    print $ length $ filter hasAllValidFields passportinfo

readPassportField :: T.Text -> PassportField
readPassportField = read . T.unpack

dataIsValid :: PassportField -> T.Text -> Bool
dataIsValid NONE = \_ -> False
dataIsValid BYR  = byrValid 
dataIsValid IYR  = iyrValid
dataIsValid EYR  = eyrValid
dataIsValid ECL  = eclValid
dataIsValid PID  = pidValid
dataIsValid HCL  = hclValid
dataIsValid HGT  = hgtValid


byrValid x = (read . T.unpack ) x `elem` [1920..2002]
iyrValid x = (read . T.unpack ) x `elem` [2010..2020]
eyrValid x = (read . T.unpack ) x `elem` [2020..2030]
eclValid x = T.unpack  x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
pidValid x = has ([regex|(\d{9}\b)|]) x
hclValid x = has ([regex|(#([0-9]|[a-f]){6}\b)|]) x
hgtValid x = (read . T.unpack ) (height x) `elem` (validHeights $ hgtTokv x)

hgtTokv x = x & [regex|(\d+)(in|cm)\b|] . match %@~ \[v, k] _ -> "{" <> k <> ":" <> v <> "}"
unitIsIn x = has ([regex|(in:)|]) x
unitIsCm x = has ([regex|(cm:)|]) x
validHeights x 
    |unitIsIn x = [59..76]
    |unitIsCm x = [150..193]
    |otherwise = []
height x    =   fromMaybe "0" (x ^? [regex|(\d+)|] . index 0 . match)

listValidFields passportinfo =  filter (uncurry dataIsValid) $ map splitFieldnameFromData $ passportFieldsWithData $ T.pack passportinfo
numValidFields  = length . listValidFields
allFieldsValid = (==7) . numValidFields
fieldNames = map fst
listHasAllFieldnames list = all (\x->x`elem` (fieldNames list)) [HCL,ECL,EYR,PID,IYR,HGT,BYR]
hasAllFieldnames  = listHasAllFieldnames . listValidFields
hasAllValidFields x = toTextAndCheckRequiredFields  x && allFieldsValid x && hasAllFieldnames x

solutionDay4b :: IO()
solutionDay4b = (splitOnBlankLine "input4.txt")  >>= print . length . filter hasAllValidFields


