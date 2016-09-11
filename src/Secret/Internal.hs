module Secret.Internal
    (
        readFileMaybe
      , getKeyfile
    )
    where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
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
