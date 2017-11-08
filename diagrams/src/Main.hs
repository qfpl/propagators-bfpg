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

twentyFour, fortyThree, seventyFive :: Maybe Double
twentyFour = Just 24
fortyThree = Just 43.2
seventyFive = Just 75.2

main :: IO ()
main = do
  dotFile "prop.dot" (simpleProp "toUpper")
  dotFile "cell1.dot" simpleCell1
  dotFile "cell2.dot" simpleCell2
  dotFile "cell3.dot" simpleCell3
  dotFile "cell4.dot" simpleCell4
  dotFile "add1.dot" (addition "" "" "")
  dotFile "add2.dot" (addition "3" "" "")
  dotFile "add3.dot" (addition "3" "4" "")
  dotFile "add4.dot" (addition "3" "4" "7")
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
  dotFile "mult1.dot" (mult "" "" "")
  dotFile "mult2.dot" (mult "5" "" "")
  dotFile "mult3.dot" (mult "5" "6" "")
  dotFile "mult4.dot" (mult "5" "6" "30")
  dotFile "bimult1.dot" (bimult "" "" "")
  dotFile "bimult1.dot" (bimult "" "" "0")
  dotFile "bimult2.dot" (bimult "BOOM" "BOOM" "0")
  dotFile "always1.dot" (always "always 3" "Nothing")
  dotFile "always2.dot" (always "always 3" "Just 3")
  dotFile "always3.dot" (always2 "always 3" "Nothing")
  dotFile "always4.dot" (always2 "always 3" "Just 3")
  dotFile "maybe1.dot" (addition "Nothing" "Nothing" "Nothing")
  dotFile "maybe2.dot" (addition "Just 3" "Nothing" "Nothing")
  dotFile "maybe3.dot" (addition "Just 3" "Just 4" "Nothing")
  dotFile "maybe4.dot" (addition "Just 3" "Just 4" "Just 7")
  dotFile "doubleplus0.dot" (doubleplus "3" "4" "" "6" "6")
  dotFile "doubleplus1.dot" (doubleplus "3" "4" "?" "6" "6")
  dotFile "doubleplus2.dot" (doubleplus "3" "4" "Contradiction" "6" "6")
  dotFile "doubleplus3.dot" (doubleplus "Just 3" "Just 4" "?" "Just 6" "Just 6")
  dotFile "doubleplus4.dot" (doubleplus "Known 3" "Known 4" "Unknown" "Known 6" "Known 6")
  dotFile "doubleplus5.dot" (doubleplus "Known 3" "Known 4" "Known 12 <> Unknown" "Known 6" "Known 6")
  dotFile "doubleplus6.dot" (doubleplus "Known 3" "Known 4" "Known 7 <> Known 12 <> Unknown" "Known 6" "Known 6")
  dotFile "doubleplus7.dot" (doubleplus "Known 3" "Known 4" "Known 7 <> Known 12" "Known 6" "Known 6")
  dotFile "doubleplus8.dot" (doubleplus "Known 3" "Known 4" "Contradiction" "Known 6" "Known 6")
  dotFile "doubleplus9.dot" (doubleplus "[3]" "[4]" "[]" "[6]" "[6]")
  dotFile "doubleplus10.dot" (doubleplus "[3]" "[4]" "[7] <> [12] <> []" "[6]" "[6]")
  dotFile "doubleplus11.dot" (doubleplus "[3]" "[4]" "[7,12]" "[6]" "[6]")
  dotFile "doubleplus12.dot" (doubleplus "[3]" "[4]" "[]" "[6]" "[6]")
  dotFile "doubleplus13.dot" (doubleplus "[3]" "[4]" "[12] <> [7] <> []" "[6]" "[6]")
  dotFile "doubleplus14.dot" (doubleplus "[3]" "[4]" "[12,7]" "[6]" "[6]")
  dotFile "doubleplus15.dot" (doubleplus "Sum 3" "Sum 4" "Sum 0" "Sum 6" "Sum 6")
  dotFile "doubleplus16.dot" (doubleplus "Sum 3" "Sum 4" "Sum 7 <> Sum 12 <> Sum 0" "Sum 6" "Sum 6")
  dotFile "doubleplus17.dot" (doubleplus "Sum 3" "Sum 4" "Sum 19" "Sum 6" "Sum 6")
  dotFile "badd5.dot" (bidirectionalAddition "Sum 3" "Sum 4" "Sum 0")
  dotFile "badd6.dot" (bidirectionalAddition "Sum 3" "Sum 4" "Sum 7 <> Sum 0")
  dotFile "badd7.dot" (bidirectionalAddition "Sum 3" "Sum 4" "Sum 7")
  dotFile "badd8.dot" (bidirectionalAddition "Sum 3 <> Sum 3" "Sum 4 <> Sum 4" "Sum 7")
  dotFile "badd9.dot" (bidirectionalAddition "Sum 6" "Sum 8" "Sum 7")
  dotFile "badd10.dot" (bidirectionalAddition "Sum 6" "Sum 8" "Sum 14 <> Sum 7")
  dotFile "badd11.dot" (bidirectionalAddition "Sum 6" "Sum 8" "Sum 21")
  dotFile "contradiction1.dot" (contradiction "Nothing" "Nothing" "Nothing")
  dotFile "contradiction2.dot" (contradiction "Just 3" "Nothing" "Just 4")
  dotFile "contradiction3.dot" (contradiction "Just 3" "      ?     " "Just 4")
  dotFile "contradiction4.dot" (contradiction "Unknown" "          Unknown          " "Unknown")
  dotFile "contradiction5.dot" (contradiction "Known 3 <> Unknown" "Known 3 <> Known 4 <> Unknown" "Known 4 <> Unknown")
  dotFile "contradiction6.dot" (contradiction "Known 3" "Known 3 <> Known 4 <> Unknown" "Known 4 <> Unknown")
  dotFile "contradiction7.dot" (contradiction "Known 3" "Known 3 <> Known 4 <> Unknown" "Known 4")
  dotFile "contradiction8.dot" (contradiction "Known 3" "Known 3 <> Known 4" "Known 4")
  dotFile "contradiction9.dot" (contradiction "Known 3" "       Contradiction       " "Known 4")
 
