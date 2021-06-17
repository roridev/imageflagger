module Common ( comma
              , uncomma
              , getCustomMaybe
              )
              where

import qualified Data.ByteString.Char8 as BC8
import Data.ByteString.Char8 (split)

comma :: [String] -> String
comma []  = []
comma [a] = a
comma (x:xs) = x ++ "," ++ comma xs

uncomma :: String -> [String]
uncomma = map  BC8.unpack . split ',' . BC8.pack

getCustomMaybe :: (Read a) => String -> Maybe a
getCustomMaybe "None" = Nothing
getCustomMaybe s = Just $ read s 
