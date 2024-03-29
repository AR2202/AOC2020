module Day12
  ( solutionDay12a
  , solutionDay12b
  ) where

import           Common
import           Data.List

-------
--Types
-------
data Orientation
  = ON
  | OS
  | OE
  | OW
  deriving (Show, Read, Eq)

type Coord = (Int, Int)

type Position = (Coord, Orientation)

data Direction
  = F
  | N
  | S
  | E
  | W
  | R
  | L
  deriving (Show, Read, Eq)

type Instruction = (Direction, Int)

type ShipWayPoint = (Coord, WayPoint)

data WayPoint =
  WayPoint
    { north :: Int
    , east  :: Int
    }
  deriving (Show, Read, Eq)

-------------------------------------
--updating position Part 1
-------------------------------------
orientation2int :: Orientation -> Int
orientation2int ON = 0
orientation2int OE = 90
orientation2int OS = 180
orientation2int OW = 270

int2orientation :: Int -> Orientation
int2orientation num
  | deg == 0 = ON
  | deg == 90 = OE
  | deg == 180 = OS
  | deg == 270 = OW
  where
    deg = num `mod` 360

move :: Instruction -> Position -> Position
move (N, num) ((x, y), o) = ((x, y - num), o)
move (S, num) ((x, y), o) = ((x, y + num), o)
move (W, num) ((x, y), o) = ((x - num, y), o)
move (E, num) ((x, y), o) = ((x + num, y), o)
move (F, num) ((x, y), o)
  | o == ON = move (N, num) ((x, y), o)
  | o == OS = move (S, num) ((x, y), o)
  | o == OW = move (W, num) ((x, y), o)
  | o == OE = move (E, num) ((x, y), o)
move (R, num) (coord, o) = (coord, newo)
  where
    newo = int2orientation $ num + orientation2int o
move (L, num) (coord, o) = move (R, 360 - num) (coord, o)

manhattenDist (x, y) = abs x + abs y

pos2dist = manhattenDist . fst

----------------------------
--Reading Input
----------------------------
solutionDay12a = do
  lines <- loadAndSplitLines "input12.txt"
  let tuples = map (splitAt 1) lines
  let instructionList = map readTuple tuples
  let startpos = ((0, 0), OE)
  let endpos = foldl' (flip move) startpos instructionList
  let dist = pos2dist endpos
  print dist

-----------------------------
--Part 2
-----------------------------
move' :: Instruction -> ShipWayPoint -> ShipWayPoint
move' (N, num) (s, WayPoint n e) = (s, WayPoint (n + num) e)
move' (S, num) (s, WayPoint n e) = (s, WayPoint (n - num) e)
move' (W, num) (s, WayPoint n e) = (s, WayPoint n (e - num))
move' (E, num) (s, WayPoint n e) = (s, WayPoint n (e + num))
move' (F, num) ((x, y), WayPoint n e) =
  ((x + num * e, y - num * n), WayPoint n e)
move' (R, num) swp = (iterate turn swp) !! (num `div` 90)
  where
    turn (s, WayPoint n e) = (s, WayPoint (-1 * e) n)
move' (L, num) x = move' (R, 360 - num) x

solutionDay12b = do
  lines <- loadAndSplitLines "input12.txt"
  let tuples = map (splitAt 1) lines
  let instructionList = map readTuple tuples
  let startpos = ((0, 0), WayPoint 1 10)
  let endpos = foldl' (flip move') startpos instructionList
  let dist = pos2dist endpos
  print dist
