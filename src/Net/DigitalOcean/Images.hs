module Net.DigitalOcean.Images (
  Image(..)
  , imgId, imgName, imgDist, imgSlug, imgPublic, imgRegions, imgCreatedAt
  , imgMinDiskSize

  , getImages
  , getDistributionImages
  , getApplicationImages
  , getImage
  , renameImage
  , deleteImage

  , transferImage
  ) where
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Network.Wreq as W
import Data.Aeson(FromJSON(..), Value(..), (.:))
import Control.Applicative
import Control.Lens hiding (Action)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError, Error)

import Net.DigitalOcean.Request (get, post, put, delete, handleResp)
import Net.DigitalOcean.Config (Config, options)
import Net.DigitalOcean.Actions (Action(..))

data Image = Image
             { _imgId :: !Int
             , _imgName :: !T.Text
             , _imgDist :: !T.Text
             , _imgSlug :: Maybe T.Text
             , _imgPublic :: !Bool
             , _imgRegions :: ![T.Text]
             , _imgCreatedAt :: !T.Text
             , _imgMinDiskSize :: !Int
             } deriving (Show, Eq)
makeLenses ''Image

instance FromJSON Image where
  parseJSON (Object x) = Image <$>
                         x .: "id" <*>
                         x .: "name" <*>
                         x .: "distribution" <*>
                         x .: "slug" <*>
                         x .: "public" <*>
                         x .: "regions" <*>
                         x .: "created_at" <*>
                         x .: "min_disk_size"
  parseJSON _ = fail "image must be object"

imagesEndpoint :: String
imagesEndpoint = "/v2/images/"

imageEndpoint :: T.Text -> String
imageEndpoint = (++) imagesEndpoint . T.unpack

actionEndpoint :: T.Text -> String
actionEndpoint n = imageEndpoint n ++ "/actions"

getImages :: (Error e, MonadError e m, MonadIO m) =>
             Config -> m [Image]
getImages = get imagesEndpoint "images"

getDistributionImages :: (Error e, MonadError e m, MonadIO m) =>
                         Config -> m [Image]
getDistributionImages c = liftIO (W.getWith opts imagesEndpoint) >>= handleResp "images"
  where opts = options c & W.param "type" .~ ["distribution"]

getApplicationImages :: (Error e, MonadError e m, MonadIO m) =>
                        Config -> m [Image]
getApplicationImages c = liftIO (W.getWith opts imagesEndpoint) >>= handleResp "images"
  where opts = options c & W.param "type" .~ ["application"]

getImage :: (Error e, MonadError e m, MonadIO m) =>
            T.Text -> Config -> m Image
getImage i = get (imageEndpoint i) "image"

renameImage :: (Error e, MonadError e m, MonadIO m) =>
               T.Text -> T.Text -> Config -> m Image
renameImage o n = put (imageEndpoint o) "image" body
  where body :: M.Map T.Text T.Text
        body = M.fromList [("name", n)]

deleteImage :: (Error e, MonadError e m, MonadIO m) =>
               T.Text -> Config -> m ()
deleteImage n = delete (imageEndpoint n)

transferImage :: (Error e, MonadError e m, MonadIO m) =>
                 T.Text -> T.Text -> Config -> m Action
transferImage n r = post (actionEndpoint n) "action" body
  where body :: M.Map T.Text T.Text
        body = M.fromList [("type", "transfer"), ("region", r)]
