module Net.DigitalOcean.Actions (
  Action(..)
  , actId, actStatus, actType, actStartedAt, actCompletedAt
  , actResourceId, actResourceType, actRegion

  , getActions
  , getAction
  ) where
import qualified Data.Text as T
import Data.Aeson(FromJSON(..), Value(..), (.:))
import Control.Applicative
import Control.Lens hiding (Action)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError, Error)

import Net.DigitalOcean.Request (get)
import Net.DigitalOcean.Config (Config)

data Action = Action
              { _actId :: !Int
              , _actStatus :: !T.Text
              , _actType :: !T.Text
              , _actStartedAt :: !T.Text
              , _actCompletedAt :: !T.Text
              , _actResourceId :: !T.Text
              , _actResourceType :: !T.Text
              , _actRegion :: !T.Text
              } deriving (Show, Eq)
makeLenses ''Action

instance FromJSON Action where
  parseJSON (Object x) = Action <$>
                         x .: "id" <*>
                         x .: "status" <*>
                         x .: "type" <*>
                         x .: "started_at" <*>
                         x .: "completed_at" <*>
                         x .: "resource_id" <*>
                         x .: "resource_type" <*>
                         x .: "region"
  parseJSON _ = fail "action must be object"

actionsEndpoint :: String
actionsEndpoint = "/v2/actions/"

actionEndpoint :: T.Text -> String
actionEndpoint = (++) actionsEndpoint . T.unpack

getActions :: (Error e, MonadError e m, MonadIO m) =>
              Config -> m [Action]
getActions = get actionsEndpoint "actions"

getAction :: (Error e, MonadError e m, MonadIO m) =>
             T.Text -> Config -> m Action
getAction n = get (actionEndpoint n) "actions"
