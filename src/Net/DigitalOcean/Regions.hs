module Net.DigitalOcean.Regions (
  Region(..)
  , rgnName, rgnSlug, rgnSizes, rgnFeatures, rgnAvailable

  , getRegions
  ) where
import qualified Data.Text as T
import Data.Aeson(FromJSON(..), Value(..), (.:))
import Control.Applicative
import Control.Lens hiding (Action)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError, Error)

import Net.DigitalOcean.Request (get)
import Net.DigitalOcean.Config (Config)


data Region = Region
              { _rgnName :: !T.Text
              , _rgnSlug :: !T.Text
              , _rgnSizes :: ![T.Text]
              , _rgnFeatures :: ![T.Text]
              , _rgnAvailable :: !Bool
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

getRegions :: (Error e, MonadError e m, MonadIO m) =>
              Config -> m [Region]
getRegions = get "/v2/regions/" "regions"
