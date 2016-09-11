module Secret.Token
    (
        getTokens
    ) where

import Prelude hiding (lookup)
import Data.ByteString.Char8 (pack)
import Data.Map (lookup)
import Web.Authenticate.OAuth

import Secret.Internal

getTokens :: IO (Maybe Credential)
getTokens = do
    m <- readFileKVMaybe =<< getKeyfile
    case m of
         Nothing -> return Nothing
         (Just d) ->
            let lookFor k = pack <$> (k `lookup` d)
                in
                return $ newCredential
                            <$> lookFor "token"
                            <*> lookFor "tokenSecret"
