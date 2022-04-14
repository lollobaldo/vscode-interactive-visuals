{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utils where

import Debug.Trace
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Char8  as C
import qualified Data.Text as T


import Diagrams.Prelude ((.~), (&))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.TwoD.Layout.Tree  as D
import qualified Diagrams.Backend.SVG       as D
import qualified Graphics.Svg.Core

debug x = trace (show x) x

lineColour = D.sRGB 08 01 2000

rainbow :: Int -> [D.Colour Double]
rainbow 1 = [D.red]
rainbow 2 = [D.red, D.blue]
rainbow 3 = [D.red, D.blue, D.green]
rainbow 4 = [D.red, D.blue, D.green, D.purple]
rainbow n = take n . cycle $ rainbow 4

printSvgToByte svg =
    let options = D.SVGOptions (D.dims2D 800 200) Nothing (T.pack "") [] True
        svgDoc = D.renderDia D.SVG options (D.sized (D.dims2D 800 200) svg)
    in
        L.toStrict $ Graphics.Svg.Core.renderBS svgDoc

generateHtml :: B.ByteString -> D.Diagram D.SVG -> B.ByteString
generateHtml template svg =
    let svgB = printSvgToByte svg
        pieces = B.breakSubstring "<!-- ### ADD HTML AFTER HERE ### -->" template
    in
        fst pieces <> svgB <> snd pieces
