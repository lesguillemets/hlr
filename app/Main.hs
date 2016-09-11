module Main where

import Flickr.Auth

main :: IO ()
main = start >>= print

