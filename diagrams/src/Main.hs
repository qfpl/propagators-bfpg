module Main where

import Control.Applicative ((<|>))
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised
import Data.Text.Lazy

propagator :: Labellable a => n -> a -> Dot n
propagator name val = node name [toLabel val, shape Square]

cell :: Labellable a => n -> a -> Dot n
cell name val = cell' name val []

cell' :: Labellable a => n -> a -> [Attribute] -> Dot n
cell' name val extras = node name $ toLabel val : extras

-- left to right
l2r :: Dot n
l2r = graphAttrs [RankDir FromLeft]

cluster_ :: Int -> DotM n a -> Dot n
cluster_ i d = cluster_' i [] d

cluster_' :: Int -> [Attribute] -> DotM n a -> Dot n
cluster_' i extras d =
  cluster (Num (Int i)) $ do
    graphAttrs (style invis : extras)
    d

simpleProp :: DotGraph String
simpleProp = digraph (Str (pack "Prop")) $ propagator "plus" "+"

simpleCell1 :: DotGraph String
simpleCell1 = digraph (Str (pack "Cell")) $ cell "cell" ""

simpleCell2 :: DotGraph String
simpleCell2 = digraph (Str (pack "Cell")) $ cell "cell" "3"

addition :: String -> String -> String -> DotGraph String
addition a b c = digraph (Str (pack "Addition")) $ do
  l2r
  cell "a" a
  cell "b" b
  cell "c" c
  propagator "plus" "+"

  "a" --> "plus"
  "b" --> "plus"
  "plus" --> "c"

bidirectionalAddition :: String -> String -> String -> DotGraph String
bidirectionalAddition a b c = digraph (Str (pack "bidirectional-addition")) $ do
  l2r

  cluster_ 0 $ do
    graphAttrs [style invis, rank MinRank, ordering OutEdges]
    cell "a" a
    cell "b" b

  cluster_ 1 $ do
    graphAttrs [style invis, rank SameRank, ordering OutEdges]
    propagator "min1" "-"
    propagator "plus" "+"
    propagator "min2" "-"
  
  cluster_ 2 $ do
    graphAttrs [style invis, rank MaxRank]
    cell "c" c
 
  edge "a" "plus" [Weight (Int 50)]
  edge "a" "min1" [Weight (Int 50)]
  edge "b" "plus" [Weight (Int 50)]
  edge "b" "min2" [Weight (Int 50)]
  "c" --> "min1"
  "c" --> "min2"
  edge "min1" "b" [Weight (Int 50)]
  edge "min2" "a" [Weight (Int 50)]
  edge "plus" "c" [Weight (Int 50)]
  invisEdge' "a" "c" [Weight (Int 0), MinLen 2]
  invisEdge' "b" "c" [Weight (Int 0), MinLen 2]

invisEdge :: (NodeList n f, NodeList n t) => f -> t -> Dot n
invisEdge s t = invisEdge' s t []

invisEdge' :: (NodeList n t, NodeList n f) => f -> t -> [Attribute] -> Dot n
invisEdge' s t extras = edge s t (style invis : extras)

celsius_ :: Maybe Double -> Bool -> Maybe Double -> DotM String ()
celsius_ c b f = do
  l2r

  cluster_ 1 $ do
    cell "c" (foldMap show c)
    propagator "times" "×"
    cell "m" $
      if b then foldMap show ((fmap (\x -> x*9/5) c) <|> fmap (\x -> x * 5/9) f) else ""
    propagator "plus" "+"
    cell "f" (foldMap show f)

  cell "nf" "9/5"
  cell "tt" "32"
  
  "c" --> "times"
  "nf" --> "times"
  "times" --> "m"
  "m" --> "plus"
  "tt" --> "plus"
  "plus" --> "f"

  invisEdge "c" "nf"
  invisEdge "nf" "m"
  invisEdge "m" "tt"
  invisEdge "tt" "f"

  -- This is a hack. xLabels go to stupid places, so hang them on invisible arrows
  edge "c" "c" [xLabel "C", penWidth 0, Dir NoDir]
  edge "f" "f" [xLabel "F", penWidth 0, Dir NoDir]

celsius :: Maybe Double -> Bool -> Maybe Double -> DotGraph String
celsius c b f = digraph (Str (pack "c2f")) $ celsius_ c b f

celsiusBi :: Maybe Double -> Bool -> Maybe Double -> DotGraph String
celsiusBi c b f = digraph (Str (pack "c2f")) $ do

  cluster_ 2 $ do
    propagator "times2" "×"
    cell "fn" "5/9"

  propagator "minus" "-"
  celsius_ c b f

  edge "f" "minus" [Weight (Int 1)]
  "tt" --> "minus"
  edge "minus" "m" [Weight (Int 1)]
  edge "m" "times2" [Weight (Int 5)]
  "fn" --> "times2"
  edge "times2" "c" [Weight (Int 5)]

dotFile :: String -> DotGraph String -> IO ()
dotFile fn = writeFile fn . unpack . printDotGraph

main :: IO ()
main = do
  dotFile "prop.dot" simpleProp
  dotFile "cell1.dot" simpleCell1
  dotFile "cell2.dot" simpleCell2
  dotFile "add1.dot" (addition "" "" "")
  dotFile "add2.dot" (addition "3" "" "")
  dotFile "add3.dot" (addition "3" "4" "")
  dotFile "add4.dot" (addition "3" "4" "7")
  dotFile "badd1.dot" (bidirectionalAddition "" "" "")
  dotFile "badd2.dot" (bidirectionalAddition "" "4" "")
  dotFile "badd3.dot" (bidirectionalAddition "" "4" "7")
  dotFile "badd4.dot" (bidirectionalAddition "3" "4" "7")
  dotFile "celsius1.dot" (celsius Nothing False Nothing)
  dotFile "celsius2.dot" (celsius (Just 24) False Nothing)
  dotFile "celsius3.dot" (celsius (Just 24) True Nothing)
  dotFile "celsius4.dot" (celsius (Just 24) True (Just 75.2))
  dotFile "celsius5.dot" (celsiusBi Nothing False Nothing)
 
