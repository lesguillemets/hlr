module Secret
    (
        KeyPair
      , getKeys
    ) where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import System.Environment
import System.Directory
import System.FilePath

data KeyPair = KeyPair Key Secret deriving (Show)
type Key = String
type Secret= String

getKeys :: IO (Maybe KeyPair)
getKeys = do
    fl <- getKeyfile >>= readKeyfile
    en <- readFromEnv
    return $ en <|> fl

getDefaultKeyFile :: IO FilePath
-- api key and api secret is assumed to stored in ~/.hlr by default.
getDefaultKeyFile = (</> ".hlr") <$> getHomeDirectory

getKeyfile :: IO FilePath
getKeyfile = do
    f <- lookupEnv "hrl_KEY_FILE"
    defaultLoc <- getDefaultKeyFile
    return . fromMaybe defaultLoc $ f

fileReadable :: FilePath -> IO Bool
fileReadable f = do
    d <- doesFileExist f
    if d
       then
       readable <$> getPermissions f
       else return False

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe f = fileReadable f >>= r
    where
        r False = return Nothing
        r True = Just <$> readFile f

readKeyfile :: FilePath -> IO (Maybe KeyPair)
readKeyfile f = (>>= parseKeyfile) <$> readFileMaybe f

parseKeyfile :: String -> Maybe KeyPair
parseKeyfile c =
    case lines c of
         (k:s:_) -> KeyPair <$> process k <*> process s
         _ -> Nothing
    where
        process = Just . unquote . dropWhile (/= '"')
        unquote = filter (/= '"')
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
