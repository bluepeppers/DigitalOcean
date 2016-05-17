{-# LANGUAGE FlexibleInstances #-}
module Net.DigitalOcean.Images (
  Image(..)
  , getImages
  , getDistributionImages
  , getApplicationImages
  , getImage
  , renameImage
  , deleteImage
  , transferImage

  -- ** Lens Accessors
  , imgId, imgName, imgDist, imgSlug, imgPublic, imgRegions, imgCreatedAt
  , imgMinDiskSize
  ) where
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Network.Wreq as W
import Data.Aeson(FromJSON(..), Value(..), (.:))
import Control.Applicative
import Control.Lens hiding (Action)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError, Error)

import Net.DigitalOcean.Request (get, post, put, delete, handleResp)
import Net.DigitalOcean.Config (Config, options)
import Net.DigitalOcean.Actions (Action(..))

-- | A Digital Ocean image
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

-- | List all images availible to the account
--
-- <https://developers.digitalocean.com/#list-all-images DO documentation>
getImages :: (Error e, MonadError e m, MonadIO m) =>
             Config -> m [Image]
getImages = get imagesEndpoint "images"

-- | List all distribution images availible to the account
--
-- <https://developers.digitalocean.com/#list-all-distribution-images DO documentation>
getDistributionImages :: (Error e, MonadError e m, MonadIO m) =>
                         Config -> m [Image]
getDistributionImages c = liftIO (W.getWith opts imagesEndpoint) >>= handleResp "images"
  where opts = options c & W.param "type" .~ ["distribution"]

-- | List all application images availible to the account
--
-- <https://developers.digitalocean.com/#list-all-application-images DO documentation>
getApplicationImages :: (Error e, MonadError e m, MonadIO m) =>
                        Config -> m [Image]
getApplicationImages c = liftIO (W.getWith opts imagesEndpoint) >>= handleResp "images"
  where opts = options c & W.param "type" .~ ["application"]

-- | Get the image with the given ID/slug
--
-- <https://developers.digitalocean.com/#retrieve-an-existing-image-by-id DO documentation>
getImage :: (Error e, MonadError e m, MonadIO m) =>
            T.Text -> Config -> m Image
getImage i = get (imageEndpoint i) "image"

-- | Rename an image
--
-- <https://developers.digitalocean.com/#update-an-image DO documentation>
renameImage :: (Error e, MonadError e m, MonadIO m) =>
               T.Text -> T.Text -> Config -> m Image
renameImage o n = put (imageEndpoint o) "image" body
  where body :: M.Map T.Text T.Text
        body = M.fromList [("name", n)]

-- | Delete an image
--
-- <https://developers.digitalocean.com/#delete-an-image DO documentation>
deleteImage :: (Error e, MonadError e m, MonadIO m) =>
               T.Text -> Config -> m ()
deleteImage n = delete (imageEndpoint n)

-- | Transfer an image to another region
--
-- <https://developers.digitalocean.com/#transfer-an-image DO documentation>
transferImage :: (Error e, MonadError e m, MonadIO m) =>
                 T.Text -> T.Text -> Config -> m Action
transferImage n r = post (actionEndpoint n) "action" body
  where body :: M.Map T.Text T.Text
        body = M.fromList [("type", "transfer"), ("region", r)]
