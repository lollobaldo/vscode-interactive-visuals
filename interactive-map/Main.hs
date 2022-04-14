{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Patterns as File

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text.Encoding
import qualified Diagrams.Prelude             as D
import Test.QuickCheck

import Visualisable
import Funcs
import List
import Protocol
import Tree
import Utils

graph :: (Visualisable t) => t -> IO ()
graph a = do
    template <- B.readFile "templates/out.template.html"
    let html = generateHtml template (visualise D.black a D.# D.lc lineColour)
    C.putStrLn $ A.encode (GraphResponse (Info "" (0, 0)) (Right (decodeUtf8 html)))

edit :: (Editable t, Show (t a), Traversable t) => Crud -> [Key] -> Maybe a -> t a -> IO ()
edit c ks mv struct = do
    let code = show $ editAtKey c ks mv (annotate struct)
    C.putStrLn $ A.encode (EditResponse (Info "" (0, 0)) (Right code))

randomPattern :: (Arbitrary (t Int), Traversable t, Visualisable1 t) => (t (Key, Colour, Int) -> t (Key, Colour, Int)) -> IO ()
randomPattern f = do
    template <- B.readFile "templates/out.template.html"
    i <- generate $ resize 5 arbitrary
    let html = generateHtml template (makePattern f i)
    C.putStrLn $ A.encode (GraphResponse (Info "" (0, 0)) (Right (decodeUtf8 html)))

sampleList = [1, 2, 3, 4] :: [Int]
pattern :: (Traversable t, Visualisable1 t) => (t (Key, Colour, Int) -> t (Key, Colour, Int)) -> t Int -> IO ()
pattern f i = do
    template <- B.readFile "templates/out.template.html"
    let html = generateHtml template (makePattern f i)
    C.putStrLn $ A.encode (GraphResponse (Info "" (0, 0)) (Right (decodeUtf8 html)))

main = do
    print "Use this module to compile and call the functions."
