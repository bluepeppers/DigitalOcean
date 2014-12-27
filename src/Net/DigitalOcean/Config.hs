module Net.DigitalOcean.Config (
  Config(..)
  , confAuth
  , options
  ) where

import qualified Network.Wreq as W
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Lens

data Config = Config { _confAuth :: !T.Text } deriving (Show, Eq)

makeLenses ''Config

options :: Config -> W.Options
options c = W.defaults & W.auth ?~ W.basicAuth (encodeUtf8 $ c ^. confAuth) ""
