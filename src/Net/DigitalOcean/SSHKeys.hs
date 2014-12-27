module Net.DigitalOcean.SSHKeys (
  SSHKey(..)

  -- ** SSHKey Actions
  , getSSHKeys
  , getSSHKey
  , createSSHKey
  , renameSSHKey
  , deleteSSHKey

  -- ** Lens Accessors
  , keyId, keyFingerprint, keyPublicKey, keyName
  ) where

import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), Value(..))
import GHC.Generics (Generic)
import Control.Lens
import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError, Error)
import qualified Data.Map as M

import Net.DigitalOcean.Request (get, post, put, delete)
import Net.DigitalOcean.Config (Config)

-- | An SSH public key stored by Digital Ocean
data SSHKey = SSHKey
              { _keyId :: !Int
              , _keyFingerprint :: !T.Text
              , _keyPublicKey :: !T.Text
              , _keyName :: !T.Text
              } deriving (Show, Eq, Generic)

makeLenses ''SSHKey

instance FromJSON SSHKey where
  parseJSON (Object x) = SSHKey <$>
                         x .: "id" <*>
                         x .: "fingerprint" <*>
                         x .: "public_key" <*>
                         x .: "name"
  parseJSON _ = fail "SSHKey can be parsed only from Object"

keysEndpoint :: String
keysEndpoint = "/v2/account/keys/"

keyEndpoint :: T.Text -> String
keyEndpoint name = keysEndpoint ++ T.unpack name

-- | Returns all SSH keys associated with the account
--
-- <https://developers.digitalocean.com/#list-all-keys DO documentation>
getSSHKeys :: (Error e, MonadError e m, MonadIO m) =>
              Config -> m [SSHKey]
getSSHKeys = get keysEndpoint "ssh_keys"

-- | Returns the ssh key with the given id or fingerprint
--
-- <https://developers.digitalocean.com/#retrieve-an-existing-key DO documentation>
getSSHKey :: (Error e, MonadError e m, MonadIO m) =>
             T.Text -> Config -> m SSHKey
getSSHKey name = get (keyEndpoint name) "ssh_key"

-- | Creates a new SSH key with the given name and contents
--
-- <https://developers.digitalocean.com/#create-a-new-key DO documentation>
createSSHKey :: (Error e, MonadError e m, MonadIO m) =>
                T.Text -> T.Text -> Config -> m SSHKey
createSSHKey name key = post keysEndpoint "ssh_key" body
  where body :: M.Map T.Text T.Text
        body = M.fromList [("name", name), ("public_key", key)]

-- | Renames an existing SSH key
--
-- <https://developers.digitalocean.com/#update-a-key DO documentation>
renameSSHKey :: (Error e, MonadError e m, MonadIO m) =>
                T.Text -> T.Text -> Config -> m SSHKey
renameSSHKey old new = put (keyEndpoint old) "ssh_key" body
  where body :: M.Map T.Text T.Text
        body = M.fromList [("name", new)]

-- | Deletes an SSH key
--
-- <https://developers.digitalocean.com/#destroy-a-key DO documentation>
deleteSSHKey :: (Error e, MonadError e m, MonadIO m) =>
                T.Text -> Config -> m ()
deleteSSHKey name = delete (keyEndpoint name)
