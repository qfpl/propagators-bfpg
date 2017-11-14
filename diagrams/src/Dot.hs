module Dot where

import Control.Monad
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised
import Data.Text.Lazy
import Data.Foldable (traverse_)

digraph_ :: String -> DotM n a -> DotGraph n
digraph_ s dotn = digraph (Str (pack s)) $ do
  graphAttrs [bgColor Transparent]
  dotn

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
simpleProp = digraph_ "Prop" . propagator "prop"

simpleCell :: String -> DotGraph String
simpleCell = digraph_ "Cell" . cell "cell"

simpleCell1 :: DotGraph String
simpleCell1 = simpleCell ""

simpleCell2 :: DotGraph String
simpleCell2 = simpleCell "3"

simpleCell3 :: DotGraph String
simpleCell3 = simpleCell "'c'"

simpleCell4 :: DotGraph String
simpleCell4 = simpleCell "Contradiction"

always :: String -> String -> DotGraph String
always p c = digraph_ "always" $ do
  l2r
  propagator "p" p
  cell "c" c
  "p" --> "c"

always2 :: String -> String -> DotGraph String
always2 p c = digraph_ "always" $ do
  l2r
  propagator "p" p
  cell "c1" c
  cell "c2" c
  "p" --> "c1"
  "p" --> "c2"

tri :: String -> String -> String -> String -> DotGraph String
tri symbol a b c = digraph_ ("tri " ++ symbol) $ do
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
bidirectional symbol1 symbol2 a b c = digraph_ ("bidirectional " ++ symbol1 ++ symbol2) $ do
  bidirectional_ symbol1 symbol2 a b c
  invisEdge' "a" "c" [Weight (Int 0), MinLen 2]
  invisEdge' "b" "c" [Weight (Int 0), MinLen 2]

bidirectionalAddition :: String -> String -> String -> DotGraph String
bidirectionalAddition = bidirectional "+" "-"

bidirectional_ :: String -> String -> String -> String -> String -> DotM String ()
bidirectional_ symbol1 symbol2 a b c = do
  l2r

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
celsius c m f = digraph_ "c2f" $ do
  l2r
  celsius_ c m f

celsiusBi :: Maybe Double -> Maybe Double -> Maybe Double -> DotGraph String
celsiusBi c m f = digraph_ "c2f" $ do
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
celsiusAdd x y c m f = digraph_ "celsiusAdd" $ do
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
doubleplus i1 i2 o j1 j2 = digraph_ "doubleplus" $ do
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
solo i p o = digraph_ "toUpper" $ do
  l2r
  cell "in" i
  propagator "prop" p
  cell "out" o

  "in" --> "prop"
  "prop" --> "out"

contradiction :: String -> String -> String -> DotGraph String
contradiction a b c = digraph_ "contradiction" $ do
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

textNode :: Labellable a => n -> a -> Dot n
textNode name val = node name [toLabel val, shape PlainText]

undirected :: (NodeList n t, NodeList n f) => f -> t -> Dot n
undirected s t = edge s t [Dir NoDir]

powerset :: DotGraph String
powerset = digraph_ "powerset" powerset_

powersetUpsideDown = digraph_ "powersetUpsideDown" $ do
  graphAttrs [RankDir FromBottom]
  powerset_

powerset_ = do
  graphAttrs [bgColor Transparent]
  textNode "empty" "{}"
  textNode "a" "{1}"
  textNode "b" "{2}"
  textNode "c" "{3}"
  textNode "d" "{4}"
  textNode "ab" "{1,2}"
  textNode "ac" "{1,3}"
  textNode "ad" "{1,4}"
  textNode "bc" "{2,3}"
  textNode "bd" "{2,4}"
  textNode "cd" "{3,4}"
  textNode "abc" "{1,2,3}"
  textNode "acd" "{1,3,4}"
  textNode "abd" "{1,2,4}"
  textNode "bcd" "{2,3,4}"
  textNode "abcd" "{1,2,3,4}"
  traverse_ (undirected "empty") ["a", "b", "c", "d"]
  traverse_ (undirected "a") ["ab", "ac", "ad"]
  traverse_ (undirected "b") ["ab", "bc", "bd"]
  traverse_ (undirected "c") ["ac", "bc", "cd"]
  traverse_ (undirected "d") ["ad", "bd", "cd"]
  traverse_ (undirected "ab") ["abc", "abd"]
  traverse_ (undirected "ac") ["abc", "acd"]
  traverse_ (undirected "ad") ["abd", "acd"]
  traverse_ (undirected "bc") ["abc", "bcd"]
  traverse_ (undirected "bd") ["abd", "bcd"]
  traverse_ (undirected "cd") ["acd", "bcd"]
  traverse (flip undirected "abcd") ["abc", "bcd", "acd", "abd"]

flat :: DotGraph String
flat = digraph_ "flat" $ do
  let theMin = -2 :: Int
  let theMax = 2 :: Int
  let theRange = (fmap show [theMin..theMax])
  let c = "Contradiction"
  let u = "Unknown"
  let tn = join textNode
  graphAttrs [bgColor Transparent]
  tn c
  textNode "eli1" "..."
  traverse_ tn theRange
  textNode "eli2" "..."
  tn u
  undirected c "eli1"
  undirected "eli1" u
  traverse_ (undirected c) theRange
  undirected c "eli2"
  undirected "eli2" u
  traverse_ (flip undirected u) theRange
  invisEdge' c u [Weight (Int 0), MinLen 2]

moreInfo :: DotGraph String
moreInfo = digraph_ "moreInfo" $ cluster (Num (Int 0)) $ do
  textNode "more" "More information"
  textNode "less" "Less information"
  edge "more" "less" [Dir Both, Style [dotted], MinLen 8]

dcpo :: DotGraph String
dcpo = digraph_ "dcpo" $ do
  let theMin = -2 :: Int
  let theMax = 2 :: Int
  let theRange = (fmap show [theMin..theMax])
  let u = "⊥"
  let tn = join textNode
  graphAttrs [bgColor Transparent]
  textNode "eli1" "..."
  traverse_ tn theRange
  textNode "eli2" "..."
  tn u
  undirected "eli1" u
  undirected "eli2" u
  traverse_ (flip undirected u) theRange

tree :: DotGraph String
tree = digraph_ "tree" $ do
  graphAttrs [RankDir FromBottom]
  cell "1" "5"
  cell "2" "2"
  cell "5+2" ""
  cell "3" "x"
  cell "4" "y"
  cell "x+y" ""
  propagator "+1" "+"
  propagator "+2" "+"
  propagator "times" "×"
  cell "out" ""

  traverse_ (--> "+1") ["1","2"]
  traverse_ (--> "+2") ["3","4"]
  "+1" --> "5+2"
  "+2" --> "x+y"
  traverse_ (--> "times") ["5+2","x+y"]
  "times" --> "out"

dotFile :: String -> DotGraph String -> IO ()
dotFile fn = writeFile fn . unpack . printDotGraph

