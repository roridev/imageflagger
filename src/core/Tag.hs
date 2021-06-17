{-# LANGUAGE OverloadedStrings #-}

module Tag ( Tag(..) )
  where

import qualified Data.Map as M 
import Hash
import Common (comma, uncomma)
import Data.ByteString.Char8 (split)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS

data Tag = Tag { unRef :: String }

instance Show Tag where
  show = unRef
instance Read Tag where
  readsPrec x s = [(Tag name, remainder)]
    where
      (name:arr) = lines s
      remainder = unlines arr 
