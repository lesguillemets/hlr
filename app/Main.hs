module Main where

import Secret
import Flickr.Auth
import Flickr.Auth.Initial

main :: IO ()
main = do
    keys <- getKeys
    case keys of
         Nothing -> putStrLn "No consumer key/secret provided"
         (Just k) -> initialise k >>= print
