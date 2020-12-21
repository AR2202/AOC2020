{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day21 
(example21a,
solutionDay21a

)
where

import Common
import Control.Lens
import Data.List (nub,(\\),foldl1',intersect)
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T

matchAllergens :: String -> [T.Text]
matchAllergens txt = concat $ T.pack txt ^.. [regex| (\w+)|] . groups

intersectAll :: Eq a => [[a]]->[a]
intersectAll  = foldl1' intersect 

differenceAll :: Eq a => [[a]]->[a]
differenceAll = foldl1' (\\)

containsAllergen allergen food = allergen `elem` snd food

ingredientsContaining foods allergen =intersectAll $ map fst $ filter (containsAllergen allergen) foods

example21a :: IO ()
example21a = part1 "example21.txt"

solutionDay21a :: IO ()
solutionDay21a = part1 "input21.txt"

part1 :: String -> IO ()
part1 filename = do
    lines <- loadAndSplitLines filename
    let ingredients = map (words . takeWhile (/= '(')) lines
    let allergens = map (matchAllergens  . dropWhile(/= '(')) lines
    let allAllergens = nub$ concat allergens
    let allIngredients =  concat ingredients
    let foods = zip ingredients allergens
    let allergensIngredients = zip allAllergens $ map (ingredientsContaining foods) allAllergens
    let allergenic = nub $ concatMap snd allergensIngredients
    let nonallergenic = length . filter (`notElem` allergenic) $ allIngredients 
    print nonallergenic

