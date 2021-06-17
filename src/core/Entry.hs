{-# LANGUAGE OverloadedStrings #-}


module Entry (Entry(..))
  where

import Data.Maybe (fromMaybe)
import Hash (Hash(..))
import Common ( comma, uncomma, getCustomMaybe )


data Entry = Entry {
  name        :: String,
  image       :: Hash,
  parents     :: [Hash],
  tags        :: [Hash],
  source      :: Maybe String,
  description :: Maybe String
}

instance Show Entry where
  show e = unlines $  map (\x -> x e)
    [name, image', parents', tags', source', desc'] 
    where
      image'   = unHash . image
      parents' = comma . map unHash . parents
      tags'    = comma . map unHash . tags
      source'  = fromMaybe "None" .  source 
      desc'    = fromMaybe "None" . description

instance Read Entry where
  readsPrec _ s = [(Entry nm image' parents' tags' source' desc', unlines xs)]
    where
      (nm:img:parn:tag:src:dsc:xs) = lines s
      image' = Hash img
      parents' = map Hash $ uncomma parn
      tags' = map Hash $ uncomma parn
      source'  = getCustomMaybe src
      desc' = getCustomMaybe dsc
      
      
  

      
