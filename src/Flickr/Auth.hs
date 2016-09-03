{-# LANGUAGE OverloadedStrings #-}
module Flickr.Auth where

import qualified Data.ByteString.Char8 as BC
import Web.Authenticate.OAuth
import Network.HTTP.Conduit

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

authenticate :: KeyPair -> IO ()
authenticate k = do
    m <- newManager tlsManagerSettings
    tempCred <- getTemporaryCredential oa m
    print tempCred
    putStrLn $ authorizeUrl oa tempCred
    verifier <- BC.getLine
    accessToken <- getAccessToken oa (injectVerifier verifier tempCred) m
    print accessToken
    where
        oa = oauth k
