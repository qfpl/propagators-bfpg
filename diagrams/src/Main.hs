module Main where

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
simpleProp = digraph (Str (pack "Prop")) $ propagator "toUpper" "toUpper"

simpleCell1 :: DotGraph String
simpleCell1 = digraph (Str (pack "Cell")) $ cell "cell" ""

simpleCell2 :: DotGraph String
simpleCell2 = digraph (Str (pack "Cell")) $ cell "cell" "3"

simpleCell3 :: DotGraph String
simpleCell3 = digraph (Str (pack "Cell")) $ cell "cell" "'c'"

always :: String -> String -> DotGraph String
always p c = digraph (Str (pack "always")) $ do
  l2r
  propagator "p" p
  cell "c" c
  "p" --> "c"

always2 :: String -> String -> DotGraph String
always2 p c = digraph (Str (pack "always")) $ do
  l2r
  propagator "p" p
  cell "c1" c
  cell "c2" c
  "p" --> "c1"
  "p" --> "c2"

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
  bidirectionalAddition_ a b c
  invisEdge' "a" "c" [Weight (Int 0), MinLen 2]
  invisEdge' "b" "c" [Weight (Int 0), MinLen 2]

bidirectionalAddition_ :: String -> String -> String -> DotM String ()
bidirectionalAddition_ a b c = do
  cluster_ 0 $ do
    graphAttrs [style invis]
    cell "a" a
    cell "b" b

  cluster_ 1 $ do
    graphAttrs [style invis]
    propagator "min1" "-"
    propagator "plus" "+"
    propagator "min2" "-"
  
  cluster_ 2 $ do
    graphAttrs [style invis]
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

invisEdge :: (NodeList n f, NodeList n t) => f -> t -> Dot n
invisEdge s t = invisEdge' s t []

invisEdge' :: (NodeList n t, NodeList n f) => f -> t -> [Attribute] -> Dot n
invisEdge' s t extras = edge s t (style invis : extras)

celsius_ :: Maybe Double -> Maybe Double -> Maybe Double -> DotM String ()
celsius_ c m f = do
  cluster_ 1 $ do
    cell "c" (foldMap show c)
    propagator "times" "×"
    cell "m" (foldMap show m)
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

celsius :: Maybe Double -> Maybe Double -> Maybe Double -> DotGraph String
celsius c m f = digraph (Str (pack "c2f")) $ do
  l2r
  celsius_ c m f

celsiusBi :: Maybe Double -> Maybe Double -> Maybe Double -> DotGraph String
celsiusBi c m f = digraph (Str (pack "c2f")) $ do
  l2r
  celsiusBi_ c m f

celsiusBi_ :: Maybe Double -> Maybe Double -> Maybe Double -> DotM String ()
celsiusBi_ c m f = do
  propagator "div" "÷"
  propagator "minus" "-"
  celsius_ c m f

  "f" --> "minus"
  "tt" --> "minus"
  "minus" --> "m"
  "m" --> "div"
  "nf" --> "div"
  "div" --> "c"

celsiusAdd :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> DotGraph String
celsiusAdd x y c m f = digraph (Str (pack "celsiusAdd")) $ do
  l2r

  celsiusBi_ c m f

  propagator "div" "÷"

  cluster_ 11 $ do
    propagator "min1" "-"
    propagator "min2" "-"
  
  cluster_ 12 $ do
    cell "b" (foldMap show y)
    cell "a" (foldMap show x)
 
  propagator "plus2" "+"

  "a" --> "min1"
  "a" --> "plus2"
  "b" --> "plus2"
  "b" --> "min2"
  "c" --> "min1"
  "c" --> "min2"
  "min1" --> "b"
  "min2" --> "a"
  "plus2" --> "c"

solo :: String -> String -> String -> DotGraph String
solo i p o = digraph (Str (pack "toUpper")) $ do
  l2r
  cell "in" i
  propagator "prop" p
  cell "out" o

  "in" --> "prop"
  "prop" --> "out"

contradiction :: String -> String -> String -> DotGraph String
contradiction a b c = digraph (Str (pack "contradiction")) $ do
  l2r

  propagator "a3" "always 3"
  propagator "a4" "always 4"

  cell "a" a
  cell "b" b
  cell "c" c

  "a3" --> "a"
  "a3" --> "b"
  "a4" --> "b"
  "a4" --> "c"

dotFile :: String -> DotGraph String -> IO ()
dotFile fn = writeFile fn . unpack . printDotGraph

twentyFour, fortyThree, seventyFive :: Maybe Double
twentyFour = Just 24
fortyThree = Just 43.2
seventyFive = Just 75.2

main :: IO ()
main = do
  dotFile "prop.dot" simpleProp
  dotFile "cell1.dot" simpleCell1
  dotFile "cell2.dot" simpleCell2
  dotFile "cell3.dot" simpleCell3
  dotFile "add1.dot" (addition "" "" "")
  dotFile "add2.dot" (addition "3" "" "")
  dotFile "add3.dot" (addition "3" "4" "")
  dotFile "add4.dot" (addition "3" "4" "7")
  dotFile "always1.dot" (always "always 3" "")
  dotFile "always2.dot" (always "always 3" "3")
  dotFile "always3.dot" (always2 "always 3" "")
  dotFile "always4.dot" (always2 "always 3" "3")
  dotFile "upper1.dot" (solo "" "toUpper" "")
  dotFile "upper2.dot" (solo "\'q\'" "toUpper" "")
  dotFile "upper3.dot" (solo "\'q\'" "toUpper" "'Q'")
  dotFile "badd1.dot" (bidirectionalAddition "" "" "")
  dotFile "badd2.dot" (bidirectionalAddition "" "4" "")
  dotFile "badd3.dot" (bidirectionalAddition "" "4" "7")
  dotFile "badd4.dot" (bidirectionalAddition "3" "4" "7")
  dotFile "celsius1.dot" (celsius Nothing Nothing Nothing)
  dotFile "celsius2.dot" (celsius twentyFour Nothing Nothing)
  dotFile "celsius3.dot" (celsius twentyFour fortyThree Nothing)
  dotFile "celsius4.dot" (celsius twentyFour fortyThree seventyFive)
  dotFile "celsius5.dot" (celsiusBi Nothing Nothing Nothing)
  dotFile "celsius6.dot" (celsiusBi Nothing Nothing seventyFive)
  dotFile "celsius7.dot" (celsiusBi Nothing fortyThree seventyFive)
  dotFile "celsius8.dot" (celsiusBi twentyFour fortyThree seventyFive)
  dotFile "celsius9.dot" (celsiusBi Nothing Nothing Nothing)
  dotFile "celsius10.dot" (celsiusBi Nothing fortyThree Nothing)
  dotFile "celsius11.dot" (celsiusBi twentyFour fortyThree seventyFive)
  dotFile "celsius12.dot" (celsiusAdd Nothing (Just 3) Nothing Nothing Nothing)
  dotFile "celsius13.dot" (celsiusAdd Nothing (Just 3) Nothing Nothing seventyFive)
  dotFile "celsius14.dot" (celsiusAdd Nothing (Just 3) Nothing fortyThree seventyFive)
  dotFile "celsius15.dot" (celsiusAdd Nothing (Just 3) twentyFour fortyThree seventyFive)
  dotFile "celsius16.dot" (celsiusAdd ((subtract 3) <$> twentyFour) (Just 3) twentyFour fortyThree seventyFive)
  dotFile "contradiction1.dot" (contradiction "" "" "")
  dotFile "contradiction2.dot" (contradiction "3" "" "4")
  dotFile "contradiction3.dot" (contradiction "3" "?" "4")
 
