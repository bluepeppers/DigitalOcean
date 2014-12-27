module Net.DigitalOcean.Config (
  Config(..)
  , confAuth
  -- * Utility/Helpers
  , options
  ) where

import qualified Network.Wreq as W
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Lens

data Config = Config { _confAuth :: !T.Text } deriving (Show, Eq)

makeLenses ''Config

-- | Generate a Wreq 'W.Options' instance from the given 'Config'. Can be
-- further modified to set additional parameters, headers, etc
options :: Config -> W.Options
options c = W.defaults & W.auth ?~ W.basicAuth (encodeUtf8 $ c ^. confAuth) ""
