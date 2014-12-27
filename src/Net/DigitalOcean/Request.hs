 {-# LANGUAGE DeriveGeneric #-}
module Net.DigitalOcean.Request (
  get,
  post,
  put,
  delete,

  -- For when the provided methods don't do enough (so quite often)
  handleResp,
  url
  ) where

import qualified Data.Text as T
import qualified Network.Wreq as W
import Data.Aeson (FromJSON, parseJSON, Value(..), eitherDecode, ToJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict as HM (lookup)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)
import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Catch (catchAll)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..), Error(..))

import Net.DigitalOcean.Config

data ResponseError = ResponseError
                     { respErr :: String
                     , respDesc :: String
                     } deriving (Show, Eq, Generic)

instance FromJSON ResponseError

baseUrl :: String
baseUrl = "https://api.digitalocean.com"

-- Get the url for an endpoint
url :: String -> String
url = (++) baseUrl

eitherToME :: (Show t, Error e, MonadError e m) => Either t a -> m a
eitherToME (Left e) = throwError . strMsg . show $ e
eitherToME (Right v) = return v

ensureValidResponse :: (Error e, MonadError e m, MonadIO m) =>
                       W.Response ByteString -> m ()
ensureValidResponse r =
  let sc = r ^. W.responseStatus . W.statusCode
  in if 200 <= sc && sc < 300
     then return ()
     else do
       let mErr :: IO ResponseError
           mErr = (liftM (view W.responseBody) (W.asJSON r))
       err <- liftIO $ catchAll mErr (throwError . strMsg . show)
       throwError . strMsg $ "Err: " ++ respErr err ++ "\nSC: " ++ show sc ++ "\nDesc: " ++
         respDesc err

handleResp :: (FromJSON a, Error e, MonadError e m, MonadIO m) =>
              String -> W.Response ByteString -> m a
handleResp key r = do
  ensureValidResponse r
  ast <- eitherToME . eitherDecode $ r ^. W.responseBody
  case ast of
    Object obj -> case HM.lookup (T.pack key) obj of
      Just v -> eitherToME $ parseEither parseJSON v
      Nothing -> throwError . strMsg $ "No key " ++ key ++ " in response"
    _ -> throwError . strMsg $ "Response was not object"


get :: (FromJSON a, Error e, MonadError e m, MonadIO m) =>
       String -> String -> Config -> m a
get e k c = liftIO (W.getWith (options c) (url e)) >>=
            handleResp k

post :: (ToJSON a, FromJSON b, Error e, MonadError e m, MonadIO m) =>
        String -> String -> a -> Config -> m b
post e k b c = liftIO (W.postWith (options c) (url e) (toJSON b)) >>=
               handleResp k

put :: (ToJSON a, FromJSON b, Error e, MonadError e m, MonadIO m) =>
       String -> String -> a -> Config -> m b
put e k b c = liftIO (W.putWith (options c) (url e) (toJSON b)) >>=
              handleResp k

delete :: (Error e, MonadError e m, MonadIO m) => String -> Config -> m ()
delete e c = liftIO (W.getWith (options c) (url e)) >>= ensureValidResponse
