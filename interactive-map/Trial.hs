{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Trial where

import Displayable
import List
import Tree
import Utils

-- import Data.Tree

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree (renderTree,symmLayout',_slHSep,_slVSep)
import Diagrams.Backend.SVG
import qualified Graphics.Svg.Core

someFunc :: IO ()
someFunc = putStrLn "someFunc"

node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices $ regPoly n 1) (map node [1..n])

example = tournament 5

-- exampleTree :: Tree String
-- exampleTree = Node "A" [Node "B" [], Node "C" []]

-- t1 = Node "Hello" [Node "Mamma" [], Node "Mamma" []]
-- t2 = Node "Hello" [Node "Mamma" [Node "Ciao" []], Node "Mamma" []]

t2 = Node "Hello" (Node "Mamma" Leaf Leaf) (Node "Hi" (Node "Ciao" Leaf Leaf) (Node "Mamma" Leaf Leaf))

outputFile :: FilePath
outputFile = "out2.html"

l1, l2 :: [Int]
l1 = []
l2 = [23, 5, 8, 28]

main = do
    print "working?"
    -- printMappedSvgToFile outputFile (prettyPrintWithMap l2)
