module Secret.Internal
    (
        readFileMaybe
      , getKeyfile
      , readFileKVMaybe
    )
    where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Data.Map (fromList, Map(..))
import Data.List (span)
import System.Environment
import System.Directory
import System.FilePath

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

readFileKVMaybe :: FilePath -> IO (Maybe (Map String String))
readFileKVMaybe f = fmap parse <$> readFileMaybe f

parse :: String -> Map String String
parse = fromList . map parseLine . lines
    where
        parseLine :: String -> (String, String)
        parseLine s =
            let (k,v') = span (== '=') s
                v = init . drop 2 $ v'
                in (k,v)
