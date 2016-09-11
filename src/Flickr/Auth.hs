{-# LANGUAGE OverloadedStrings #-}
module Flickr.Auth
    (
        start
    )
    where


import Web.Authenticate.OAuth

import Flickr.Auth.Initial
import Flickr.Auth.Common
import Secret.Consumer (getKeys, KeyPair)
import Secret.Token (getTokens)

start :: IO (Maybe (OAuth, Credential))
start = do
    keys <- getKeys
    case keys of
         Nothing -> do
             putStrLn "No consumer key/secret provided"
             return Nothing
         (Just k) -> do
             cr <- getCreds k
             return $ Just (oauth k, cr)

getCreds :: KeyPair -> IO Credential
getCreds k = do
    creds <- getTokens
    case creds of
         Nothing -> initialise k
         (Just c) -> return c
