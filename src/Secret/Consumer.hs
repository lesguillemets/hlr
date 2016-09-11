module Secret.Consumer
    (
        KeyPair(..)
      , getKeys
    ) where

import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Data.Map (Map(..), lookup)
import System.Environment
import System.Directory
import System.FilePath

import Secret.Internal

data KeyPair = KeyPair Key Secret deriving (Show)
type Key = String
type Secret= String

getKeys :: IO (Maybe KeyPair)
getKeys = do
    fl <- getKeyfile >>= readKeyfile
    en <- readFromEnv
    return $ en <|> fl

readKeyfile :: FilePath -> IO (Maybe KeyPair)
readKeyfile f = (>>= parseKeyfile) <$> readFileKVMaybe f

parseKeyfile :: Map String String -> Maybe KeyPair
parseKeyfile m =
    KeyPair <$> lookFor "key" <*> lookFor "secret"
    where
        lookFor = (`lookup` m)
-- sample:
-- $ cat ~/.hlr
-- key="01234ggggg"
-- secret="0123ffff"
-- TODO : ignoring key= part

readFromEnv :: IO (Maybe KeyPair)
readFromEnv = do
    apiKey <- lookupEnv "hlr_KEY"
    apiSecret <- lookupEnv "hlr_SECRET"
    return $ liftM2 KeyPair apiKey apiSecret

-- gk :: IO (Maybe (Key, Secret))
-- gk = (<|>) <$> (getKeyfile >>= readKeyfile) <*> readFromEnv
