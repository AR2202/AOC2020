module Day24
(move,
Direction(..),
test24,
example24a,
solutionDay24a

)
where

import Common
import Text.Parsec (ParseError,parse,eof,try,string)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Char
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Either.Unwrap(fromRight,isRight)
import Data.List(nub)

---------------------
--Types
---------------------

data Direction = E | SE | SW | W | NW | NE deriving (Show, Read, Eq)

type Coord = (Int,Int)

------------------------
--Identifying tiles
------------------------

move (x,y) E = (x+2,y)
move (x,y) W = (x-2,y)
move (x,y) NE = (x+1,y-1)
move (x,y) NW = (x-1,y-1)
move (x,y) SE = (x+1,y+1)
move (x,y) SW = (x-1,y+1)

findTile = foldl move (0,0) 

isBlackTile :: Eq a=> [a] -> a -> Bool
isBlackTile tilelist tile = (numflips tilelist tile) `mod` 2 == 1

numflips tilelist tile = length $ filter (== tile ) tilelist

blackTiles tilelist = filter (isBlackTile tilelist) $ nub tilelist
---------------------------------------
--Parsing
---------------------------------------




parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""



parseE :: Parser Direction
parseE = do
    e <- string "e"
    return E

parseSE :: Parser Direction
parseSE = do
    se <- string "se"
    return SE

parseSW :: Parser Direction
parseSW = do
    sw <- string "sw"
    return SW

parseW :: Parser Direction
parseW = do
    w <- string "w"
    return W

parseNE :: Parser Direction
parseNE = do
    ne <- string "ne"
    return NE

parseNW :: Parser Direction
parseNW = do
    sw <- string "nw"
    return NW

parseDirection :: Parser [Direction]
parseDirection = do
   
    e <- dir 
    maybeAddSuffix [e]
  where
    dir =  try parseE <|> try parseNE <|> try parseNW <|> try parseSW <|> try parseW <|> try parseSE
    addSuffix e0 = do   
        e1 <- dir 
        maybeAddSuffix  ( e1:e0 )
    
    maybeAddSuffix e = addSuffix e  <|> return (reverse e)

stringToTile dirstring = findTile <$> parseWithEof parseDirection dirstring

----------------
--example
----------------

example24 = "nwwswee"

test24 = stringToTile example24

example24a :: IO ()
example24a = part1 "example24.txt"
--------------
--Part 1
--------------

solutionDay24a :: IO ()
solutionDay24a = part1 "input24.txt"

part1 :: String -> IO ()
part1 filename = do
    directions <- loadAndSplitLines filename
    let tiles = map stringToTile directions
    let blacktiles = blackTiles tiles
    print $ length blacktiles


