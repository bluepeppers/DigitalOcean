module Net.DigitalOcean.Sizes (
  Size(..)
  , getSizes

  -- * Lense Accessors
  , szSlug
  , szTransfer
  , szPriceMonthly
  , szPriceHourly
  , szMemory
  , szVCPUs
  , szDisk
  , szRegions
  ) where
import qualified Data.Text as T
import Data.Aeson(FromJSON(..), Value(..), (.:))
import Control.Applicative
import Control.Lens hiding (Action)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError, Error)

import Net.DigitalOcean.Request (get)
import Net.DigitalOcean.Config (Config)

-- | A potential size of a Digital Ocean droplet
data Size = Size
            { _szSlug :: !T.Text
            , _szTransfer :: !Int
            , _szPriceMonthly :: !Double
            , _szPriceHourly :: !Double
            , _szMemory :: !Int
            , _szVCPUs :: !Int
            , _szDisk :: !Int
            , _szRegions :: ![T.Text]
            } deriving (Show, Eq)
makeLenses ''Size

instance FromJSON Size where
  parseJSON (Object x) = Size <$>
                         x .: "slug" <*>
                         x .: "transfer" <*>
                         x .: "price_monthly" <*>
                         x .: "price_hourly" <*>
                         x .: "memory" <*>
                         x .: "vcpus" <*>
                         x .: "disk" <*>
                         x .: "regions"
  parseJSON _ = fail "size must be object"

-- | List all potential sizes
--
-- <https://developers.digitalocean.com/#list-all-sizes DO documentation>
getSizes :: (Error e, MonadError e m, MonadIO m) =>
            Config -> m [Size]
getSizes = get "/v2/size/" "sizes"
