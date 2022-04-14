{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module List where

import Data.Maybe

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Visualisable
import Utils

-- instance Displayable [] where
--   display = prettyPrintList . annotate
--   generate n = zip (rainbow n) [0..]

instance Visualisable1 [] where
  visualise1 = visualiseList

instance Editable [] where
  editAtKey Create = appendToStart
  editAtKey Update = editListAtKey
  editAtKey Delete = editListAtKey

visualiseList :: (Colour -> a -> D.Diagram D.SVG) -> [(Key, Info, a)] -> D.Diagram D.SVG
visualiseList f =  D.lc lineColour . snd . foldr (connectElems . visElem) mempty
  where
    visElem (n, c, x) = (show n, f c x # D.named (show n) # D.svgId (show n) # D.svgClass "clickable")
    connectElems :: (String, D.Diagram D.SVG) -> (String, D.Diagram D.SVG) -> (String, D.Diagram D.SVG)
    connectElems (n, svg) (n', svg') = (n, D.hsep 1 [svg, svg'] # D.connectOutside' (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15) n n')

appendToStart :: [Key] -> Maybe a -> [(Key, a)] -> [a]
appendToStart _ (Just a) as = a : [a | (_, a) <- as]

editListAtKey :: [Key] -> Maybe a -> [(Key, a)] -> [a]
editListAtKey [k] mv = go
  where
      go ((i, x):xs)
        | i == k    = maybe mempty return mv <> map snd xs
        | otherwise = return x <> go xs


-- createAtKeyM :: (Applicative t, Foldable t, Monoid (t a)) => Crud -> [Key] -> Maybe a -> t (Key, a) -> t a
-- createAtKeyM Create [k] (Just v) = foldMap (\(i, e) -> if i == k then pure v <> pure e else pure e)

-- updateAtKeyM :: (Applicative t, Foldable t) => Crud -> [Key] -> Maybe a -> t (Key, a) -> t a
-- updateAtKeyM Update [k] (Just v) = fmap (\(i, e) -> if i == k then v else e)

-- deleteAtKeyM :: (Applicative t, Foldable t, Monad t) => Crud -> [Key] -> Maybe a -> t (Key, a) -> t a
-- deleteAtKeyM Delete [k] Nothing = (=<<) (\(i, e) -> if i == k then mempty else pure e)
-- editMonadAtKey _ _ _ = error "Not implemented"
