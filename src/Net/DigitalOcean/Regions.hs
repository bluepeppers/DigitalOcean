{-# LANGUAGE FlexibleInstances #-}
module Net.DigitalOcean.Regions (
  Region(..)
  , getRegions

  -- ** Lens Accessors
  , rgnName, rgnSlug, rgnSizes, rgnFeatures, rgnAvailable
  ) where
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson(FromJSON(..), Value(..), (.:), fromJSON)
import Control.Applicative
import Control.Lens hiding (Action)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError, Error)

import Net.DigitalOcean.Request (get)
import Net.DigitalOcean.Config (Config)

-- | A Digital Ocean region
data Region = Region
              { _rgnName :: !T.Text
              , _rgnSlug :: !T.Text
              , _rgnSizes :: ![T.Text]
              , _rgnFeatures :: ![T.Text]
              , _rgnAvailable :: Maybe Bool
              } deriving (Show, Eq)
makeLenses ''Region

instance FromJSON Region where
  parseJSON (Object x) = Region <$>
                         x .: "name" <*>
                         x .: "slug" <*>
                         x .: "sizes" <*>
                         x .: "features" <*>
                         x .: "available"
  parseJSON _ = fail "region must be object"

-- | Returns a list of all the visible Digital Ocean regions
--
-- <https://developers.digitalocean.com/#list-all-regions DO documentation>
getRegions :: (Error e, MonadError e m, MonadIO m) =>
              Config -> m [Region]
getRegions = get "/v2/regions/" "regions"
