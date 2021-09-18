{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day20
  ( example20
  , solutionDay20a
  ) where

import           Common
import           Control.Lens
import           Control.Lens.Regex.Text
import           Data.List               (nub, (\\))
import           Data.Maybe              (fromJust)
import qualified Data.Set                as S
import qualified Data.Text               as T
import           Text.RawString.QQ

example20 :: IO ()
example20 = part1 "example20.txt"

solutionDay20a :: IO ()
solutionDay20a = part1 "input20.txt"

part1 :: String -> IO ()
part1 filename = do
  tiles <- splitOnBlankLine filename
  let borders = map makeBorder tiles
  let tileids = map makeTileId tiles
  let tilelist = zip tileids borders
  let corners = map fst $ filter (sndIsCornerTile borders) tilelist
  let result = product corners
  print result

matchDigits :: String -> Maybe T.Text
matchDigits txt = T.pack txt ^? [regex|(\d+)|] . match

readToInt :: T.Text -> Int
readToInt txt = read $ T.unpack txt

makeBorder line = [top, bottom, right, left]
  where
    top = head $ drop 1 $ lines line
    bottom = last $ lines line
    right = map last $ drop 1 $lines line
    left = map head $ drop 1 $lines line

makeTileId line = readToInt $ fromJust $ matchDigits $ head $ lines line

isCornerTile :: [[String]] -> [String] -> Bool
isCornerTile tilelist tile = (length . filter notMatching) tile == 2
  where
    notMatching x = x `notElem` tileset && reverse x `notElem` tileset
    tileset = nub $ concat $ tilelist \\ [tile]

sndIsCornerTile tilelist tile = isCornerTile tilelist $ snd tile
