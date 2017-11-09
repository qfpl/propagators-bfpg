{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diag where

import Data.Semigroup
import Data.Typeable

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text as DT (Text)

renderDiagram :: FilePath -> Maybe Double -> Maybe Double -> QDiagram B V2 Double Any -> IO ()
renderDiagram fp x y = renderSVG fp (mkSizeSpec (V2 x y))

invisible = lw none

screen :: (V t ~ V2, TrailLike t) => t
screen = rect 16 9

hgap = rect 16 1
ihgap = invisible hgap

screenTv =
  (text "TV" <> ihgap)
  ===
  screen
  ===
  ihgap

screenHyp ::
  ( Renderable (Path V2 n) b
  , Renderable (DT.Text n) b
  , RealFloat n
  , Typeable n
  ) => QDiagram b V2 n Any
screenHyp = 
  let vs = trailVertices screen
      c1 = vs !! 3
      c2 = vs !! 1
  in  (text "TV" <> ihgap)
      ===
      (text "55\"" <> arrowBetween c1 c2 <> screen)
      ===
      ihgap

screenWidth ::
  ( Renderable (Path V2 n) b
  , Renderable (DT.Text n) b
  , RealFloat n
  , Typeable n
  ) => QDiagram b V2 n Any
screenWidth =
  let vs = trailVertices hgap
      c1 = vs !! 3
      c2 = vs !! 0
  in  (text "Projector Screen" <> ihgap)
      ===
      screen
      ===
      (fontSize (local 50) (text "10'") <> arrowBetween c1 c2 <> ihgap)

screenVariables = mempty

