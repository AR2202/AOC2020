module Day25 
(
example25a,
solutionDay25a
)
where

import Common

---------------
--transformations
----------------

transformOnce subjectNumber value = value' `mod` 20201227
    where value' = subjectNumber * value

transformNTimes subjectNumber value n = iterate (transformOnce subjectNumber) value !! n

transformNTimes' = transformNTimes 7

findLoopNumber endval = transformUntil  1 0 
    where transformUntil value loop
            |value == endval = loop
            |otherwise = transformUntil (transformOnce 7 value) (loop + 1) 

findEncryptionKey publicKey1 publicKey2 = transformNTimes publicKey1  1 loopNum2
    where loopNum2 = findLoopNumber publicKey2

--------------
--example
--------------

cardPublicKeyE = 5764801
doorPublicKeyE = 17807724

example25a :: IO ()
example25a = print $ findEncryptionKey cardPublicKeyE doorPublicKeyE

----------
--Part1
----------

solutionDay25a :: IO ()
solutionDay25a = do
    keys <- loadAndSplitLines "input25.txt"
    let doorPublicKey = read $ head keys
    let cardPublicKey = read $ last keys
    let encryptionKey = findEncryptionKey  cardPublicKey doorPublicKey
    print encryptionKey