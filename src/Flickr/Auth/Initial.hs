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

import Secret (KeyPair(..))

oauth :: KeyPair -> OAuth
oauth (KeyPair key secret) =
    newOAuth {
        oauthServerName = "https://www.flickr.com/",
        oauthConsumerKey = BC.pack key,
        oauthConsumerSecret = BC.pack secret,
        oauthSignatureMethod = HMACSHA1,
        oauthRequestUri =
            "https://www.flickr.com/services/oauth/request_token",
        oauthAuthorizeUri =
            "https://www.flickr.com/services/oauth/authorize",
        oauthAccessTokenUri =
            "https://www.flickr.com/services/oauth/access_token",
        oauthCallback = Just "oob"
    }

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
