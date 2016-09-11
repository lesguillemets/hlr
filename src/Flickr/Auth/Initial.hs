{-# LANGUAGE OverloadedStrings #-}
module Flickr.Auth.Initial
    (
        authenticateInitial
    )
    where

import qualified Data.ByteString.Char8 as BC
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import System.IO (hFlush, stdout)

import Flickr.Auth.Common
import Secret (KeyPair(..))

authenticateInitial :: KeyPair -> IO Credential
authenticateInitial k = do
    m <- newManager tlsManagerSettings
    tempCred <- getTemporaryCredential oa m
    putStr "Access the link to authenticate:\n\t"
    putStrLn $ authorizeUrl oa tempCred
    putStr "PIN? \t" <* hFlush stdout
    verifier <- BC.getLine
    getAccessToken oa (injectVerifier verifier tempCred) m
    where
        oa = oauth k
