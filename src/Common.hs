{-# LANGUAGE ScopedTypeVariables #-}
module Common
(loadInput,
loadAndSplitLines,
splitOnBlankLine,
readLines,
loadPadded

)
where

import Control.Applicative
import Control.Monad
import Data.List.Split

dir = "input/"
filepath filename = dir ++ filename

loadInput filename = readFile $ filepath filename

loadAndSplitLines filename = do
    contents <- loadInput filename
    let linesOfFile = lines contents
    return linesOfFile

readLines :: Read a => String -> IO [a]
readLines filename = map read <$> loadAndSplitLines filename


splitOnBlankLine filename = splitOn "\n\n" <$> loadInput filename

loadPadded filename = do
    contents <- loadInput filename
    let linesOfFile = lines contents
    let linesWPadding = map (\line -> '.':line ++ ".") linesOfFile

    let len = length $ head linesOfFile
    let paddingline = replicate len '.'
    let paddingadded = paddingline : linesWPadding ++[paddingline]
    return paddingadded