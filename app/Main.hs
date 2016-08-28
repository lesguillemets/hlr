module Main where

import Secret

main :: IO ()
main = getKeys >>= print
