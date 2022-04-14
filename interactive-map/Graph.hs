{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}


module Graph where

import Data.Maybe
import Data.Graph
import Control.Monad (liftM, ap)
import Debug.Trace

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.Backend.SVG       as D
import qualified Diagrams.TwoD.Layout.Tree  as D

import Displayable
import Utils

-- instance {-# OVERLAPPABLE #-} Displayable (Graph) where
--   display = fst . prettyPrintGraphWithMap

-- instance Editable Graph where
--   editAtKey = editGraphAtKey

-- editGraphAtKey :: Graph -> Key -> Maybe a -> Graph
-- editGraphAtKey graph k mv = graph

-- prettyPrintGraphWithMap :: Graph ->  (D.Diagram D.SVG, Map)
-- prettyPrintGraphWithMap g = (tournament n # D.connectOutside' opt (1 :: Int) (2 :: Int), [])
--   where
--     opt = (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15 )
--     n = length $ vertices g
--     vs = vertices g
--     tournament n = D.atPoints (D.trailVertices $ D.regPoly n 1) (map node vs)
--     node n = D.text (show n) # D.fontSizeL 0.2 # D.fc D.white <> D.circle 0.2 # D.fc D.green # D.named n
