{-# LANGUAGE OverloadedStrings #-}

module Hash ( Hash(..)
            , hash
            , readFromHash
            , writeToHash
            , derivePath
            )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BSC8
import qualified Crypto.Hash.SHA1 as SHA1
import System.Directory

newtype Hash = Hash {unHash :: String }

instance Show Hash where
  show = unHash

instance Read Hash where
  readsPrec _ input  = [(Hash x, unlines xs)]
    where
      (x:xs) = lines input

hash :: (Show a) => a -> Hash
hash = Hash . BSC8.unpack . BS16.encode . SHA1.hash . BSC8.pack . show

readFromHash :: (Read a) => String -> Hash -> IO a
readFromHash path hash' = read <$> readFile filepath
  where
    filepath = path ++ snd (derivePath hash')

writeToHash :: (Show a) => String -> a -> IO ()
writeToHash path a = sequence_ [createDirectoryIfMissing True dir, writeFile filepath $ show a]
  where
    derived = derivePath $ hash a
    filepath = path ++ snd derived
    dir = path ++ fst derived

derivePath :: Hash -> (String,String)
derivePath h = (start, start ++ "/" ++ text)
  where
    text = unHash h
    start  = [head text,text!!1]
