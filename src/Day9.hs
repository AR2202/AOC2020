module Day9
    ( firstNotSumex,
    solutionDay9a,
    solutionDay9b,
    example9b

    ) where

import Data.List (intersect, nub,sort)
import Common

diffToNum :: [Int] -> Int -> [Int]
diffToNum list num = map (num -) list


findDiffToNumInList :: [Int] -> Int -> [Int]
findDiffToNumInList list num 
    |(num `mod` 2)== 0 = intersect list $ diffToNum (filter (/= (num `div`2)) list) num
    |otherwise= intersect list $ diffToNum list num


isNotSumOf2Nums list n = [] == findDiffToNumInList (previous25 n list) (list !!n)


previous25 n = take 25 . drop (n-25)

firstNotSum list =  list !! (indInvalid list)

indInvalid list = head $ filter (isNotSumOf2Nums list) [25..length list -1]

----------------------
--example
---------------------
example9 = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
isNotSumOf2Numsex list n = [] == findDiffToNumInList (previous5 n list ) (list !!n)

previous5 n = take 5 . drop (n-5)

firstNotSumex =  example9 !! ind
    where ind = head $ filter (isNotSumOf2Numsex example9) [5..length example9 -1]

-------------------
--Part 1
---------------------

solutionDay9a = do
    numberlist <-readLines "input9.txt"
    let result = firstNotSum numberlist
    print result

-------------
--Part 2
-------------

takeDrop n m list = (take n . drop m ) list

allStretches list ind = [takeDrop n m list | n<-[0..ind], m <- [0..ind], n+m <= ind]

strechSumsToNum list ind = filter (\l-> sum l == list!!ind) $ allStretches list ind
addSmallestLargest list = head sortedlist + last sortedlist
    where sortedlist = sort list

encryptionWeakness list ind = addSmallestLargest $ head $ strechSumsToNum list ind

example9b = encryptionWeakness example9 14

solutionDay9b = do
    numberlist <-readLines "input9.txt"
    let ind = indInvalid numberlist
    let result = encryptionWeakness numberlist ind
    print result