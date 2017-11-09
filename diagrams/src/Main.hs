{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diag
import Dot

twentyFour, fortyThree, seventyFive :: Maybe Double
twentyFour = Just 24
fortyThree = Just 43.2
seventyFive = Just 75.2

main :: IO ()
main = do
  dotFiles
  diagrams

diagrams :: IO ()
diagrams = do
  renderDiagram "screen1.svg" (Just 400) (Just 400) screenTv
  renderDiagram "screen2.svg" (Just 400) (Just 400) screenHyp
  renderDiagram "screen3.svg" (Just 400) (Just 400) screenWidth
  renderDiagram "screen4.svg" (Just 400) (Just 400) screenVariables

dotFiles :: IO ()
dotFiles = do
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
 
