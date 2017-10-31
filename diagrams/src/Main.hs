--{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised
import Data.Text.Lazy

addition :: String -> String -> String -> DotGraph String
addition a b c = digraph (Str (pack "Addition")) $ do
  graphAttrs [RankDir FromLeft]
  node "a" [toLabel a]
  node "b" [toLabel b]
  node "c" [toLabel c]
  node "plus" [toLabel "+", shape Square]

  "a" --> "plus"
  "b" --> "plus"
  "plus" --> "c"

bidirectionalAddition :: String -> String -> String -> DotGraph String
bidirectionalAddition a b c = digraph (Str (pack "bidirectional-addition")) $ do
  graphAttrs [RankDir FromLeft]

  cluster (Num (Int 0)) $ do
    graphAttrs [style invis, rank MinRank, ordering OutEdges]
    node "a" [toLabel a]
    node "b" [toLabel b]

  cluster (Num (Int 1)) $ do
    graphAttrs [style invis, rank SameRank, ordering OutEdges]
    node "min1" [toLabel "-", shape Square]
    node "plus" [toLabel "+", shape Square]
    node "min2" [toLabel "-", shape Square]
  
  cluster (Num (Int 2)) $ do
    graphAttrs [style invis, rank MaxRank]
    node "c" [toLabel c]
 
  edge "a" "plus" [Weight (Int 50)]
  edge "a" "min1" [Weight (Int 50)]
  edge "b" "plus" [Weight (Int 50)]
  edge "b" "min2" [Weight (Int 50)]
  "c" --> "min1"
  "c" --> "min2"
  edge "min1" "b" [Weight (Int 50)]
  edge "min2" "a" [Weight (Int 50)]
  edge "plus" "c" [Weight (Int 50)]
  edge "a" "c" [style invis, Weight (Int 0), MinLen 2]
  edge "b" "c" [style invis, Weight (Int 0), MinLen 2]

dotFile :: String -> DotGraph String -> IO ()
dotFile fn = writeFile fn . unpack . printDotGraph

main :: IO ()
main = do
  dotFile "add1.dot" (addition "" "" "")
  dotFile "add2.dot" (addition "3" "" "")
  dotFile "add3.dot" (addition "3" "4" "")
  dotFile "add4.dot" (addition "3" "4" "7")
  dotFile "badd1.dot" (bidirectionalAddition "" "" "")
  dotFile "badd2.dot" (bidirectionalAddition "" "4" "")
  dotFile "badd3.dot" (bidirectionalAddition "" "4" "7")
  dotFile "badd4.dot" (bidirectionalAddition "3" "4" "7")
 
