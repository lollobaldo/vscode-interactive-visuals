{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Protocol where

import GHC.Generics
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import Data.Text

import Data.Aeson

type Html = Text
type Code = String
type Error = String

data Info = Info String (Int, Int) deriving (Generic, Show)

data GraphResponse = GraphResponse Info (Either Error Html) deriving (Generic, Show)

data EditResponse = EditResponse Info (Either Error Code) deriving (Generic, Show)

instance ToJSON Info where
  toEncoding (Info s (l, r)) = pairs ("identifier" .= s <> "start" .= l <> "end" .= r)

instance ToJSON GraphResponse where
  toEncoding (GraphResponse i (Left e)) = pairs ("info" .= i <> "error" .= e)
  toEncoding (GraphResponse i (Right h)) = pairs ("info" .= i <> "html" .= h)

instance ToJSON EditResponse where
  toEncoding (EditResponse i (Left e)) = pairs ("info" .= i <> "error" .= e)
  toEncoding (EditResponse i (Right c)) = pairs ("info" .= i <> "code" .= c)

