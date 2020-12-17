module Day17 
(example17,
solutionDay17a,
example17b,
solutionDay17b

)
where

import Common
import Control.Comonad.Store
import Data.List

------------Types--------------------------------
data Cube = Inactive | Active deriving (Show, Eq, Read)

type Coord3 = (Int,Int,Int)

type Coord4 = (Int,Int,Int,Int)



--preparing Start Values-----------------------------------

startingCubes :: [Coord3] -> Store Coord3 Cube
startingCubes active = mkStore active (0,0,0)

checkCube :: Eq a =>[a] -> a -> Cube
checkCube active coord
  |coord `elem` active = Active
  |otherwise = Inactive


mkStore :: Eq a=>[a] -> a -> Store a Cube
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

-----------------------------------
--Part 2
------------------------------------




--logic of updating one cube-------------------------

  
neighbours4D (x,y,z,w) = filter (/= (x,y,z,w)) [(x1,y1,z1,w1)| x1<-[x-1..x+1],y1<-[y-1..y+1],z1<-[z-1..z+1],w1<-[w-1..w+1]]



activeNeighbours4 :: Coord4 -> [Coord4] -> Int
activeNeighbours4 coord active = length . filter (`elem` active) $ neighbours4D coord

neighboursToCubeState4 :: Coord4 -> [Coord4]-> Cube
neighboursToCubeState4 cube active
   |liveNs <  2 = Inactive
   |liveNs >  3 = Inactive
   |liveNs == 3 = Active
   |liveNs == 2 =  if cubeState == Active then Active else Inactive
   where  cubeState = checkCube active cube
          liveNs    = activeNeighbours4 cube active

          
--taking one step-------------------------------------------------


step4D ::  [Coord4] ->  [Coord4]
step4D active =  filter isActive $ nub $ active ++ activeNs
    where activeNs = active >>= neighbours4D
          isActive cube = neighboursToCubeState4 cube active == Active


nSteps4D n active = (iterate step4D active) !! n

activeAfterNSteps4D n active = length  $ nSteps4D n active



--------reading Input--------------



toCube4D x (a,b) = ((a,x,1,1),toCubeState b)
toCubes4D x list = map (toCube4D x) list

numActiveCubes4D filename = do
    lines<-loadAndSplitLines filename
    let xvals = map (zip [0..]) lines
    let cubes = concat $ zipWith toCubes4D [0..] xvals
    let active = map fst $ filter (\t -> snd t == Active) cubes
    let activeAfter6 = activeAfterNSteps4D 6 active
    print activeAfter6

example17b = numActiveCubes4D "example17.txt"
solutionDay17b = numActiveCubes4D "input17.txt"