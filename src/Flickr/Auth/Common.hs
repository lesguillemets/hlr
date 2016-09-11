{-# LANGUAGE OverloadedStrings #-}
module Flickr.Auth.Common
    (
        oauth
    )
    where

import qualified Data.ByteString.Char8 as BC
import Web.Authenticate.OAuth

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
