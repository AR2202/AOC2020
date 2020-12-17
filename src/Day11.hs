{-#LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable#-}
module Day11
(example11_1,
solutionDay11a

)
where
import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Functor.Rep
import Data.Distributive
import Data.Maybe
import Common
import qualified Data.Vector as V
import Data.Function (fix)

--Types-------

data SeatState = Occupied | Empty | None deriving (Show, Eq, Read)
type Coord = (Int,Int)
type Seat = (Coord,SeatState)
type Seats = [Seat]
type Seatsvec = [(Int,SeatState)]
newtype Coordvec a = Coordvec (V.Vector a) deriving (Eq, Show, Read, Functor, Foldable)

gridsizeEx = 10201
lineSizeEx = 101



instance Distributive Coordvec where
    distribute = distributeRep

instance Representable Coordvec where
    type Rep Coordvec = Int
    index (Coordvec v) i =  v V.! (i `mod` gridsizeEx)
    tabulate g = Coordvec $ V.generate gridsizeEx g
    
--preparing Start Values-----------------------------------



checkSeat :: Seatsvec -> Int -> SeatState
checkSeat seats ind = fromMaybe None (lookup ind seats )

mkStore :: Seatsvec ->  Store Coordvec SeatState
mkStore seats = store checkFunction 0
    where checkFunction = checkSeat seats
    
    
    
    
--logic of updating one seat-------------------------

coord2vecind (x,y) = y*lineSizeEx + x
vecind2coord ind = (x,y)
    where x = ind `mod` lineSizeEx
          y = ind `div` lineSizeEx

seat2vec (coord,state) = (coord2vecind coord,state)

seats2seatsvec = map seat2vec    
      
neighbours (x,y) = filter (/= (x,y)) [(x1,y1)| x1<-[x-1..x+1],y1<-[y-1..y+1]]

neighboursInt ind =  fmap   coord2vecind $ neighbours $ vecind2coord ind
    
    
checkNeighbours :: Store Coordvec SeatState -> [SeatState]
checkNeighbours grid = experiment neighboursInt grid
    
occupiedNeighbours :: Store Coordvec SeatState  -> Int
occupiedNeighbours = length. (filter (== Occupied) ) . checkNeighbours
    
neighboursToSeat :: Store Coordvec SeatState  -> SeatState
neighboursToSeat store
    | (seatState == Empty)    && (occupiedNs == 0) = Occupied
    | (seatState == Occupied) && (occupiedNs >= 4) = Empty
    | otherwise                                    = seatState
    where  seatState  =  extract store

           occupiedNs = occupiedNeighbours store
              
    
    
--taking one step-------------------------------------------------
    
    
step :: Store Coordvec SeatState -> Store Coordvec SeatState
step store = extend neighboursToSeat store


-------loading input-------------------
example11_1 = numOccupiedSeats "example11_1.txt"


solutionDay11a = numOccupiedSeats "input11.txt"

numOccupiedSeats filename = do
    lines<-loadPadded filename
    let xvals = map (zip [0..]) lines
    let seats = concat $ zipWith toSeats [0..] xvals
    let seatsAsVec = seats2seatsvec seats
    let store = mkStore seatsAsVec 
    let coords = coordsFromSeats seatsAsVec
    let steadyState = equalsPrevious $  fmap (coordsToSeats coords) $ (steps store)
    let numOccupied = occupiedSeats steadyState
    print $ numOccupied

replaceSnd x (a,b)  = (a,x)

replaceSndList x list = map (replaceSnd x) list

toSeatState '#' = Occupied
toSeatState 'L' = Empty
toSeatState _   = None

toSeat x (a,b) = ((a,x),toSeatState b)

toSeats x list = map (toSeat x) list

coordsFromSeats seats = map fst seats

coordsToSeats coords store  = fmap (flip peek store) coords

steps store = iterate step store

equalsPrevious list 
    |head list == (head . tail) list = head list
    |otherwise               = equalsPrevious (tail list)


occupiedSeats = length . filter   (== Occupied)


