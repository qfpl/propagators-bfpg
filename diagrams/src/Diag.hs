{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diag where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

screen :: Diagram B
screen = rect 16 9
