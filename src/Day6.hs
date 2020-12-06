module Day6 
(solutionDay6a
)
where

import Common
import Data.List
import Control.Monad



solutionDay6a :: IO()
solutionDay6a = splitOnBlankLine "input6.txt" >>= print . sum . fmap  (length . nub  . join . lines)
    