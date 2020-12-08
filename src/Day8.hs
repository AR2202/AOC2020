{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day8 
(
example8,
solutionDay8a)
where

import Common
import Control.Lens
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T
import Text.Read
import qualified Data.List as L


data Instruction = NOP | ACC | JMP deriving (Show, Read, Eq)
type InstructionLine = (Instruction, Int)
type Ind = Int
type Program = [InstructionLine]
data ProgramState = ProgramState {currIndex :: Ind,
                     currAcc :: Int,
                     previouslyRun :: [Ind]} deriving (Show, Read, Eq)

runInstructions :: Program -> ProgramState -> Int
runInstructions prog progstate 
    |currIndex progstate `elem` previouslyRun progstate = currAcc progstate
    |currInstruction == NOP = runInstructions prog nextline
    |currInstruction == ACC = runInstructions prog accUpdated
    |currInstruction == JMP = runInstructions prog jumpline
        where 
            currInstruction = fst $ prog !! (currIndex progstate)
            currInt = snd $ prog !! (currIndex progstate)
            nextline = ProgramState (currIndex progstate +1)
                                    (currAcc progstate)
                                    ((currIndex progstate) : (previouslyRun progstate))
            accUpdated = ProgramState (currIndex progstate +1)
                                      (currAcc progstate + currInt)
                                      ((currIndex progstate) : (previouslyRun progstate))
            jumpline = ProgramState (currIndex progstate +currInt)
                                    (currAcc progstate)
                                    ((currIndex progstate) : (previouslyRun progstate))


initialState :: ProgramState
initialState = ProgramState 0 0 []



readInput line =   T.pack line ^.. ([regex|(\w+) ([\+|-]\d)+|] . group 0)
readNumber line =   T.pack line ^.. ([regex|(\w+) ([\+|-]\d+)|] . group 1)

readAsInstruction :: T.Text -> Instruction
readAsInstruction = read . T.unpack . T.toUpper 

readAsInt :: T.Text -> Int
readAsInt = read .dropPlus.  T.unpack 

instructions lines = map readAsInstruction $ concatMap readInput lines

numbers lines = map readAsInt $ concatMap readNumber lines

toInstructionLines lines = zip (instructions lines) (numbers lines)

dropPlus z = if "+" `L.isPrefixOf` z then tail z else z

applyProgram :: String -> IO ()
applyProgram input = do
    lines <-loadAndSplitLines input
    let prog = toInstructionLines lines
    let accumulator = runInstructions prog initialState
    
    print accumulator

example8 :: IO ()
example8 = applyProgram "example8.txt"

solutionDay8a :: IO ()
solutionDay8a = applyProgram "input8.txt"

