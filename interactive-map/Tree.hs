{-# LANGUAGE OverloadedStrings #-}

module Tree where

import Data.Maybe
import Data.Tree
import Control.Monad (liftM, ap)
import Debug.Trace

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.Backend.SVG       as D
import qualified Diagrams.TwoD.Layout.Tree  as D

import Visualisable
import Utils
import Data.Graph (Tree)

instance Visualisable1 Tree where
  visualise1 = visualiseTree

instance Editable Tree where
  -- editAtKey = editTreeAtKey
  editAtKey Create = undefined
  editAtKey Update = undefined
  editAtKey Delete = undefined

visualiseTree :: (Colour -> a -> D.Diagram D.SVG) -> Tree (Key, Info, a) -> D.Diagram D.SVG
visualiseTree f tree = D.renderTree r (~~) posAnnTree
  where
    r (k, c, e) =  D.boundingRect (f c e # D.pad 1.1) D.<> f c e # D.svgId (show k) # D.svgClass "clickable" # D.center
    posAnnTree = D.symmLayout' (D.with & D.slHSep .~ 2 & D.slVSep .~ 2) tree

editTreeAtKey :: Crud -> [Key] -> Maybe a -> Tree (Key, a) -> Tree a
editTreeAtKey op [k] mv = go
  where
    go (Node (i, x) sub)
      | i == k    = if isNothing mv then Node x [] else Node (fromJust mv) (map (fmap snd) sub)
      | otherwise = Node x (map go sub)

-- visualiseTree :: (a -> D.Diagram D.SVG) -> Tree (Key, Info, a) -> D.Diagram D.SVG
-- visualiseTree f tree = D.renderTree ((<> D.circle 1 # D.fc D.white) . r) (~~) posAnnTree
--   where
--     r (k, c, e) = D.svgId (show k) $ D.svgClass "clickable" (f e)
--     posAnnTree = D.symmLayout' (D.with & D.slHSep .~ 4 & D.slVSep .~ 4) tree

-- editTreeAtKey Create [k] mv (Node e ns) = foldMap (\(i, e) -> if i == k then pure v <> pure e else pure e)
-- editTreeAtKey Update = 
-- editTreeAtKey Delete = 
