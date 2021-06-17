module Repository ()
  where

import Data.Map (Map)
import qualified Data.Map as M

import qualified Hash as H -- OH NO!
import qualified Entry as E -- lel
import qualified Tag as T -- this wasn't intentional lmao
import Common (comma, uncomma)

data Repository = Repository {
  root :: H.Hash,
  head' :: H.Hash,
  objects :: [H.Hash],
  tags :: Map H.Hash T.Tag,
  entries :: Map H.Hash E.Entry,
  index :: Map String [H.Hash]
}

readIndex :: String -> IO (Map String [H.Hash])
readIndex path = do
  file <- readFile path
  let (lx:xs) = lines file
  pure M.empty

instance Show Repository where
  show r = unlines $ map (\x -> x r)
    [root', head'', objects', tags', entries']
    where
      root' = H.unHash . root
      head'' = H.unHash . head'
      objects' = comma . map H.unHash . objects
      tags'    = comma . map H.unHash . M.keys . tags
      entries' = comma . map H.unHash . M.keys . entries

instance Read Repository where
  readsPrec _ s = [(Repository root' head'' objects' tags' entries' index', unlines xs)]
    where
      (rt:hd:obj:tg:entr:xs) = lines s
      root' = H.Hash rt
      head'' = H.hash hd
      objects' = map H.Hash $ uncomma obj
      tags' = undefined
      entries' = undefined
      index' = undefined
