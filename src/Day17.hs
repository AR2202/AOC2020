module Day17 
(example17,
solutionDay17a

)
where

import Common
import Control.Comonad.Store
import Data.List

------------Types--------------------------------
data Cube = Inactive | Active deriving (Show, Eq, Read)

type Coord3 = (Int,Int,Int)

type Coord4 = (Int,Int,Int)



--preparing Start Values-----------------------------------

startingCubes :: [Coord3] -> Store Coord3 Cube
startingCubes active = mkStore active (0,0,0)

checkCube :: [Coord3] -> Coord3 -> Cube
checkCube active coord
  |coord `elem` active = Active
  |otherwise = Inactive


mkStore :: [Coord3] -> Coord3 -> Store Coord3 Cube
mkStore active ind = store checkFunction ind
  where checkFunction = checkCube active





--logic of updating one cube-------------------------

  
neighbours (x,y,z) = filter (/= (x,y,z)) [(x1,y1,z1)| x1<-[x-1..x+1],y1<-[y-1..y+1],z1<-[z-1..z+1]]



checkNeighbours :: Store Coord3 Cube -> [Cube]
checkNeighbours cubes = experiment neighbours cubes

activeNeighbours :: Store Coord3 Cube -> Int
activeNeighbours = length. filter (== Active).checkNeighbours

neighboursToCubeState :: Store Coord3 Cube -> Cube
neighboursToCubeState cube
   |liveNs <  2 = Inactive
   |liveNs >  3 = Inactive
   |liveNs == 3 = Active
   |liveNs == 2 =  if cubeState == Active then Active else Inactive
   where  cubeState = extract cube
          liveNs    = activeNeighbours cube
          


--taking one step-------------------------------------------------


step :: (Store Coord3 Cube, [Coord3]) -> (Store Coord3 Cube, [Coord3])
step (cubes, active) = (newCubes, newActive)
  where newCubes  = updateStore active $ extend neighboursToCubeState cubes
        newActive = updateActiveCubes active newCubes

updateActiveCubes :: [Coord3] -> Store Coord3 Cube -> [Coord3]
updateActiveCubes active currentCells = filter (\x->(peek x currentCells) == Active) $ nub $ active ++ activeNeighbours
  where activeNeighbours = active >>= neighbours 


updateStore currentLiving currentCells = mkStore newLiving ind
  where newLiving = updateActiveCubes currentLiving currentCells
        ind       = pos currentCells

nSteps n store = (iterate step store )!! n

activeAfterNSteps n store = length . snd $ nSteps n store

--------reading Input--------------

toCubeState '#' = Active
toCubeState _   = Inactive

toCube x (a,b) = ((a,x,1),toCubeState b)

toCubes x list = map (toCube x) list

------------------------------------
--Part1
------------------------------------
numActiveCubes filename = do
    lines<-loadAndSplitLines filename
    let xvals = map (zip [0..]) lines
    let cubes = concat $ zipWith toCubes [0..] xvals
    let active = map fst $ filter (\t -> snd t == Active) cubes
    let store = startingCubes active 
    let activeAfter6 = activeAfterNSteps 6 (store,active)
    print activeAfter6

example17 = numActiveCubes "example17.txt"
solutionDay17a = numActiveCubes "input17.txt"