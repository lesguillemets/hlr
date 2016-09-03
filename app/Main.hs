module Main where

import Secret
import Flickr.Auth

main :: IO ()
main = do
    keys <- getKeys
    case keys of
         Nothing -> putStrLn "No consumer key/secret provided"
         (Just k) -> authenticate k
