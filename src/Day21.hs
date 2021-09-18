{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day21
  ( example21a
  , solutionDay21a
  , solutionDay21b
  , example21b
  ) where

import           Common
import           Control.Lens
import           Control.Lens.Regex.Text
import           Data.List               (foldl1', intersect, nub, sort, (\\))
import           Data.String.Utils       (join)
import qualified Data.Text               as T
import           Text.RawString.QQ

---------------
--Part 1
---------------
matchAllergens :: String -> [T.Text]
matchAllergens txt = concat $ T.pack txt ^.. [regex| (\w+)|] . groups

intersectAll :: Eq a => [[a]] -> [a]
intersectAll = foldl1' intersect

differenceAll :: Eq a => [[a]] -> [a]
differenceAll = foldl1' (\\)

containsAllergen allergen food = allergen `elem` snd food

ingredientsContaining foods allergen =
  intersectAll $ map fst $ filter (containsAllergen allergen) foods

example21a :: IO ()
example21a = part1 "example21.txt"

solutionDay21a :: IO ()
solutionDay21a = part1 "input21.txt"

part1 :: String -> IO ()
part1 filename = do
  lines <- loadAndSplitLines filename
  let ingredients = map (words . takeWhile (/= '(')) lines
  let allergens = map (matchAllergens . dropWhile (/= '(')) lines
  let allAllergens = nub $ concat allergens
  let allIngredients = concat ingredients
  let foods = zip ingredients allergens
  let allergensIngredients =
        zip allAllergens $ map (ingredientsContaining foods) allAllergens
  let allergenic = nub $ concatMap snd allergensIngredients
  let nonallergenic = length . filter (`notElem` allergenic) $ allIngredients
  print nonallergenic

-------------
--Part 2
-------------
singles = filter (\x -> (length (snd x)) == 1)

intersectSecond list2 list1 = (fst list1, (snd list1) \\ list2)

findAllergens ::
     [(T.Text, [String])] -> [(T.Text, [String])] -> [(T.Text, [String])]
findAllergens [] !identified = identified
findAllergens !unidentified !identified =
  findAllergens newunidentified (newidentified ++ identified)
  where
    newidentified = singles unidentified
    newunidentified =
      map (intersectSecond (nub (concatMap snd newidentified))) $ unidentified \\
      newidentified

example21b :: IO ()
example21b = part2 "example21.txt"

solutionDay21b = part2 "input21.txt"

part2 :: String -> IO ()
part2 filename = do
  lines <- loadAndSplitLines filename
  let ingredients = map (words . takeWhile (/= '(')) lines
  let allergens = map (matchAllergens . dropWhile (/= '(')) lines
  let allAllergens = nub $ concat allergens
  let foods = zip ingredients allergens
  let allergensIngredients =
        zip allAllergens $ map (ingredientsContaining foods) allAllergens
  let identifiedAllergens = sort $ findAllergens allergensIngredients []
  let dangerouslist = concatMap snd identifiedAllergens
  let dangerousstring = join "," dangerouslist
  putStrLn dangerousstring
