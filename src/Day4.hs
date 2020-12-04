{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day4
(
solutionDay4a)
where

import Common
import Control.Lens
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T
import Text.Read

passportFields txt =   txt ^.. [regex|\b(byr|iyr|eyr|hgt|hcl|ecl|pid):|] . match

hasAllRequiredFields = (==7) . length . passportFields 

toTextAndCheckRequiredFields = hasAllRequiredFields . T.pack


solutionDay4a :: IO()
solutionDay4a = (splitOnBlankLine "input4.txt")  >>= print . length . filter toTextAndCheckRequiredFields 

    