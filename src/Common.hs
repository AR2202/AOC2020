{-# LANGUAGE ScopedTypeVariables #-}
module Common
(loadInput,
loadAndSplitLines,
readLines

)
where

import Control.Applicative
import Control.Monad

dir = "input/"
filepath filename = dir ++ filename

loadInput filename = readFile $ filepath filename

loadAndSplitLines filename = do
    contents <- loadInput filename
    let linesOfFile = lines contents
    return linesOfFile

readLines :: Read a => String -> IO [a]
readLines filename = map read <$> loadAndSplitLines filename