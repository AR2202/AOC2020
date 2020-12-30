{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day14
(

solutionDay14a

)
where

import Common
import Control.Applicative((<|>))
import Data.IntMap.Strict as IM
import Data.Maybe
import Data.List as L
import Data.List.Split
import Control.Lens
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T

overwrite s r = readBinaryDigit s <|> readBinaryDigit r

readBinaryDigit :: Char -> Maybe Char
readBinaryDigit '0' = Just '0'
readBinaryDigit '1' = Just '1'
readBinaryDigit _   = Nothing

readChar2digit '1' = 1
readChar2digit '0' = 0

showDigit2Char 0 = '0'
showDigit2Char 1 = '1'

applyMask =  zipWith overwrite


masked mask string = string2decimal $ Prelude.map fromJust $ applyMask mask string

readWithMask mask string = masked mask $ i36tobinary $ read string

maskedValues (masklist,list) = Prelude.map ((readWithMask (masklist !!1)) . (\l ->l!! 1)) list

memAddresses2Keys (masklist,list) = Prelude.map (memAddress2key . head) list

memAddress2key :: String -> Int
memAddress2key string = read $ T.unpack $ head $toListOf([regex|(\d+)|] . Control.Lens.Regex.Text.group 0) $ T.pack string

string2decimal "" = 0
string2decimal string = (readChar2digit . head) string *2^(length string - 1) + string2decimal (tail string)

i36tobinary i = decimal2binary i 35
    where
        decimal2binary _ (-1) = ""
        decimal2binary i n = show (i `div` (2^n)) ++ decimal2binary (i `mod` (2^n))(n-1)


solutionDay14a :: IO ()
solutionDay14a = do
    instructions <-splitOnEq "input14.txt"
    let masks = Prelude.filter (\list -> head list == "mask") instructions
    let addresses = splitWhen (\list -> head list == "mask") instructions
    let masksAddresses = zip masks (tail addresses)
    let maskedVals = concatMap maskedValues masksAddresses
    let keys = concatMap memAddresses2Keys masksAddresses
    let keysVals = zip keys maskedVals
    let intmap = L.foldl' (flip(uncurry IM.insert)) IM.empty keysVals
    let sumNums = IM.foldl' (+) 0 intmap
    print sumNums