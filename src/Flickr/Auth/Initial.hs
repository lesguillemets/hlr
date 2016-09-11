{-# LANGUAGE OverloadedStrings #-}
module Flickr.Auth.Initial
    (
        initialise
    )
    where

import qualified Data.ByteString.Char8 as BC
import Data.Monoid ((<>))
import Data.Map (fromList, (!))
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import System.IO (hFlush, stdout, appendFile)
import System.Directory
import System.FilePath

import Flickr.Auth.Common
import Secret.Consumer (KeyPair(..))

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

initialise :: KeyPair -> IO Credential
initialise k = do
    accessToken <- authenticateInitial k
    let
        cds = fromList $ unCredential accessToken
        tkn = cds ! "oauth_token"
        tknscr = cds ! "oauth_token_secret"
    store <- (</> ".hlr") <$> getHomeDirectory
    BC.appendFile store . (\s -> "token=\"" <> s <> "\"\n") $ tkn
    BC.appendFile store . (\s -> "tokenSecret=\"" <> s <> "\"") $ tknscr
    return accessToken
