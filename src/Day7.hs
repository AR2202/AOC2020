{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day7 
(
example7,
solutionDay7a)
where

import Data.Graph
import Data.Maybe
import Data.List
import Data.List.Split
import Common
import Control.Lens
import Control.Lens.Regex.Text
import Text.RawString.QQ
import qualified Data.Text as T
import Text.Read




splitColors line =  toListOf([regex|(\w+\s\w+) bags?|] . Control.Lens.Regex.Text.group 0) $ T.pack line

colorListToTriple colorlist=  (head colorlist, head colorlist,  tail colorlist)

reachableDay7a filename = do
    sentences <- loadAndSplitLines filename
    let colors = map splitColors sentences
    let triples = map colorListToTriple colors
    let (examplegraph2, nodefromvertex2,vertexfromkey2) = graphFromEdges triples
    let transposedgraph = transposeG examplegraph2
    let reachableFromShinyGold = reachable transposedgraph (fromJust $ vertexfromkey2 "shiny gold")
    let numReachable = length reachableFromShinyGold -1

    print numReachable

example7 = reachableDay7a "example7.txt"
solutionDay7a = reachableDay7a "input7.txt"