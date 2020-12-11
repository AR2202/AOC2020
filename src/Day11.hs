module Day11
(example11_1,
solutionDay11a

)
where

import Control.Comonad.Store
import Common
import qualified Data.Vector as V
import Data.Function (fix)

data SeatState = Occupied | Empty | None deriving (Show, Eq, Read)

type Coord = (Int,Int)
type Seat = (Coord,SeatState)
type Seats = [Seat]
    

    
--preparing Start Values-----------------------------------
    
startingCond :: Seats -> Store Coord Seat
startingCond seats = mkStore seats (0,0)
    
checkSeat :: Seats -> Coord ->  Seat
checkSeat seats coord = case (lookup coord seats) of 
    Nothing -> (coord, None)
    Just x  -> (coord, x)
    
    
mkStore :: Seats -> Coord -> Store Coord Seat
mkStore seats coord = store checkFunction coord
    where checkFunction = checkSeat seats
    
    
    
    
--logic of updating one seat-------------------------
    
      
neighbours (x,y) = filter (/= (x,y)) [(x1,y1)| x1<-[x-1..x+1],y1<-[y-1..y+1]]
    
    
    
checkNeighbours :: Store Coord Seat -> Seats
checkNeighbours store = experiment neighbours store
    
occupiedNeighbours :: Store Coord Seat  -> Int
occupiedNeighbours = length. (filter (\seat -> (snd seat )== Occupied)) .checkNeighbours
    
neighboursToSeat :: Store Coord Seat  -> Seat
neighboursToSeat store
    | (seatState == Empty)    && (occupiedNs == 0) = (coord,Occupied)
    | (seatState == Occupied) && (occupiedNs >= 4) = (coord,Empty)
    | otherwise                                    = (coord,seatState)
    where  seatState  = (snd . extract) store
           coord      = (fst . extract) store
           occupiedNs = occupiedNeighbours store
              
    
    
--taking one step-------------------------------------------------
    
    
step :: Store Coord Seat -> Store Coord Seat
step store = extend neighboursToSeat store


-------loading input-------------------
example11_1 = numOccupiedSeats "example11_1.txt"

--unfortunately can't be run on the input as too inefficient
solutionDay11a = numOccupiedSeats "input11.txt"

numOccupiedSeats filename = do
    lines<-loadAndSplitLines filename
    let xvals = map (zip [0..]) lines
    let seats = concat $ zipWith toSeats [0..] xvals
    let store = mkStore seats (0,0)
    let coords = coordsFromSeats seats
    let steadyState = equalsPrevious $  map (coordsToSeats coords) $ (steps store)

    let numOccupied = occupiedSeats steadyState
    print $ numOccupied

replaceSnd x (a,b)  = (a,x)

replaceSndList x list = map (replaceSnd x) list

toSeatState '#' = Occupied
toSeatState 'L' = Empty
toSeatState _   = None

toSeat x (a,b) = ((a,x),toSeatState b)

toSeats x list = map (toSeat x) list

coordsFromSeats seats= map fst seats

coordsToSeats coords store  = map (flip peek store) coords

steps store = iterate step store

equalsPrevious list 

    |head list == (head . tail) list = head list
    |otherwise               = equalsPrevious (tail list)

equalsPrevious' list recFn n

    |list !!n == list !!(n-1) = list!!n
    |otherwise               = recFn (n+1)

slowEqualsPrevious list = fix $ equalsPrevious' list

occupiedSeats = length. filter (\seat -> (snd seat) == Occupied)

eqprevFaster list x = fn (\n -> v V.! n)
    where v = V.generate x $fn (\n -> v V.! n)
          fn = equalsPrevious' list

