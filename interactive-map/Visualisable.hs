{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ExistentialQuantification #-}

module Visualisable where

import Data.List
import Control.Monad.State.Lazy
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Utils

type Point = (Int, Int)
type Rect = (Point, Point)
type Map = [(String, String)]

type Key = Int
type Label = Int
type Info = D.Colour Double
type Colour = D.Colour Double

data Crud = Create | Update | Delete deriving (Show, Eq)
class Editable t where
  editAtKey :: Crud -> [Key] -> Maybe a -> t (Key, a) -> t a

class Visualisable a where
  visualise :: Colour -> a -> D.Diagram D.SVG

class Visualisable1 t where
  visualise1 :: (Colour -> a -> D.Diagram D.SVG) -> t (Key, Info, a) -> D.Diagram D.SVG

instance (Traversable t, Visualisable1 t, Visualisable a, D.IsName a) => Visualisable (t a) where
  visualise _ = visualise1 visualise . fmap (\(k, a) -> (k, D.orange, a)) . annotate

showDisplay :: Show a => Info -> a -> D.Diagram D.SVG
showDisplay color t = D.text t' <> D.rect (0.8 * genericLength t') 1.2 D.# D.fc color
    where
      t' = show t

-- instance Visualisable Char where
--   visualise = showDisplay D.black

instance Visualisable String where
  visualise = showDisplay

instance {-# OVERLAPPABLE  #-} (Show a) => Visualisable a where
  visualise = showDisplay


annotate :: (Traversable t) => t a -> t (Key, a)
annotate t = evalState (traverse go t) 0
  where
    go :: a -> State Key (Key, a)
    go a = do n <- get
              put (n+1)
              return (n, a)

-- createAtKeyM :: (Applicative t, Foldable t, Monoid (t a)) => Crud -> [Key] -> Maybe a -> t (Key, a) -> t a
-- createAtKeyM Create [k] (Just v) = foldMap (\(i, e) -> if i == k then pure v <> pure e else pure e)

-- updateAtKeyM :: (Applicative t, Foldable t) => Crud -> [Key] -> Maybe a -> t (Key, a) -> t a
-- updateAtKeyM Update [k] (Just v) = fmap (\(i, e) -> if i == k then v else e)

-- deleteAtKeyM :: (Applicative t, Foldable t, Monad t) => Crud -> [Key] -> Maybe a -> t (Key, a) -> t a
-- deleteAtKeyM Delete [k] Nothing = (=<<) (\(i, e) -> if i == k then mempty else pure e)
-- editMonadAtKey _ _ _ = error "Not implemented"
