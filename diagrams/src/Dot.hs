module Dot where

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

labelledCell :: Labellable a => String -> a -> Dot String
labelledCell name val = do
  cell name val
  invisEdge name name

cell' :: Labellable a => n -> a -> [Attribute] -> Dot n
cell' name val extras = node name $ toLabel val : extras

-- left to right
l2r :: Dot n
l2r = graphAttrs [RankDir FromLeft]

(<--) :: (NodeList n a) => a -> a -> Dot n
t <-- s = edge t s [Dir Back]

cluster_ :: Int -> DotM n a -> Dot n
cluster_ i d = cluster_' i [] d

cluster_' :: Int -> [Attribute] -> DotM n a -> Dot n
cluster_' i extras d =
  cluster (Num (Int i)) $ do
    graphAttrs (style invis : extras)
    d

simpleProp :: String -> DotGraph String
simpleProp = digraph (Str (pack "Prop")) . propagator "prop"

simpleCell :: String -> DotGraph String
simpleCell = digraph (Str (pack "Cell")) . cell "cell"

simpleCell1 :: DotGraph String
simpleCell1 = simpleCell ""

simpleCell2 :: DotGraph String
simpleCell2 = simpleCell "3"

simpleCell3 :: DotGraph String
simpleCell3 = simpleCell "'c'"

simpleCell4 :: DotGraph String
simpleCell4 = simpleCell "Contradiction"

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

tri :: String -> String -> String -> String -> DotGraph String
tri symbol a b c = digraph (Str (pack ("tri " ++ symbol))) $ do
  l2r
  cell "a" a
  cell "b" b
  cell "c" c
  propagator "symbol" symbol

  "a" --> "symbol"
  "b" --> "symbol"
  "symbol" --> "c"

addition :: String -> String -> String -> DotGraph String
addition = tri "+"

mult :: String -> String -> String -> DotGraph String
mult = tri "×"

bimult :: String -> String -> String -> DotGraph String
bimult = bidirectional "*" "÷"

bidirectional :: String -> String -> String -> String -> String -> DotGraph String
bidirectional symbol1 symbol2 a b c = digraph (Str (pack ("bidirectional " ++ symbol1 ++ symbol2))) $ do
  bidirectional_ symbol1 symbol2 a b c
  invisEdge' "a" "c" [Weight (Int 0), MinLen 2]
  invisEdge' "b" "c" [Weight (Int 0), MinLen 2]

bidirectionalAddition :: String -> String -> String -> DotGraph String
bidirectionalAddition = bidirectional "+" "-"

bidirectional_ :: String -> String -> String -> String -> String -> DotM String ()
bidirectional_ symbol1 symbol2 a b c = do
  cluster_ 0 $ do
    graphAttrs [style invis]
    cell "a" a
    cell "b" b

  cluster_ 1 $ do
    graphAttrs [style invis]
    propagator "min1" symbol2
    propagator "plus" symbol1
    propagator "min2" symbol2
  
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

doubleplus :: String -> String -> String -> String -> String -> DotGraph String
doubleplus i1 i2 o j1 j2 = digraph (Str (pack "doubleplus")) $ do
  cell "i1" i1
  cell "i2" i2
  propagator "plus1" "+"
  cell "o" o
  propagator "plus2" "+"
  cell "j1" j1
  cell "j2" j2

  "i1" --> "plus1"
  "i2" --> "plus1"
  "plus1" --> "o"
  "o" <-- "plus2"
  "plus2" <-- "j1"
  "plus2" <-- "j2"

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

