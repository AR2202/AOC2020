{-#LANGUAGE FlexibleContexts#-}
module Day2
(
solutionDay2a

)
where

import Text.Regex.Posix
import Data.Char
import Common

example1d2 = ["1-3 a: abcde", "1-3 b: cdefg","2-9 c: ccccccccc"]

isValid (lowerBound,upperBound, char, password) 
    | upperBound >= numOccurrances && numOccurrances >= lowerBound = True
    | otherwise                                  = False
        where numOccurrances = length. filter (==char)$ password

isValidPassword :: String -> Bool
isValidPassword = isValid . passwordConditions

numValidPasswords :: [String] -> Int
numValidPasswords = length . filter isValidPassword

firstOfTriple (a,b,c) = a
secondOfTriple (a,b,c) = b
thirdOfTriple (a,b,c) = c

extractPassword string =  string =~ ": " :: (String,String,String)
extractChar string = string =~ "([a-z])":: (String,String,String)
extractLowerBound string = string =~ "-":: (String,String,String)
extractUpperBound string = string =~ "([0-9]*)":: (String,String,String)

passwordInString string = thirdOfTriple $ extractPassword string
charInString string = head $ secondOfTriple $ extractChar string 
lowerInString string = read $ firstOfTriple $ extractLowerBound string ::Int
upperInString string = read $ secondOfTriple $ extractUpperBound $ thirdOfTriple $extractLowerBound string::Int

passwordConditions string = (lowerInString string, upperInString string, charInString string, passwordInString string)

solutionDay2a :: IO ()
solutionDay2a = do
    passwords <- loadAndSplitLines "input2.txt"
    let result = numValidPasswords passwords
    print result
