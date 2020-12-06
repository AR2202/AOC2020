{-# LANGUAGE ScopedTypeVariables #-}
module Common
(loadInput,
loadAndSplitLines,
splitOnBlankLine,
readLines

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