{-# LANGUAGE ScopedTypeVariables #-}

module Funcs where

import Data.Maybe

import Test.QuickCheck
import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Visualisable
import List
import Utils

type FuncIn a = (Key, Colour, a)

-- rand :: [FuncIn]
-- rand = zip (rainbow 5) [1,2,3, 4, 5]

-- makePattern :: ([Int] -> [Int]) -> D.Diagram D.SVG
-- makePattern f = display D.black (f rand)

makePattern :: (Traversable t, Visualisable1 t) => (t (Key, Colour, Int) -> t (Key, Colour, Int)) -> t Int -> D.Diagram D.SVG
makePattern f i = D.hsep 1 [visualise1 visualise patterned, visualise1 visualise (f patterned)]
  where
    colours = rainbow $ length i
    patterned = (\(k, e) -> (k, colours !! k, e)) <$> annotate i

-- f :: Show a => a -> a
-- f = id

-- main = do
--     i :: Show a => a <- generate arbitrary
--     let a = f i
--     print a
