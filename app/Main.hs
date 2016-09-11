module Main where

import Secret.Consumer (getKeys)
import Secret.Token (getTokens)
import Flickr.Auth
import Flickr.Auth.Initial

main :: IO ()
main = do
    keys <- getKeys
    case keys of
         Nothing -> putStrLn "No consumer key/secret provided"
         (Just k) -> do
             creds <- getTokens
             case creds of
                  Nothing -> initialise k >>= print
                  (Just c) -> print c
