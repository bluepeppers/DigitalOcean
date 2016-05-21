{-# LANGUAGE FlexibleInstances #-}
module Net.DigitalOcean.Droplets (
  Network
  , Kernel
  , Droplet
  , Snapshot
  , Backup
  , DropletAction(..)
  , DropletCreationOpts

  , createDroplet
  , getDroplet
  , getDroplets
  , getDropletKernels
  , getDropletSnapshots
  , getDropletBackups
  , getDropletActions
  , deleteDroplet
  , performDropletAction

  -- * Droplet creation options
  , defaultDCO

  -- * Lens Accessors
  , netProto
  , netIpAddr
  , netNetmask
  , netGateway
  , netType

  , krnId
  , krnName
  , krnVersion

  , drpId
  , drpName
  , drpMemory
  , drpVCPUs
  , drpDisk
  , drpLocked
  , drpCreatedAt
  , drpStatus
  , drpBackupIds
  , drpSnapshotIds
  , drpFeatures
  , drpRegion
  , drpImage
  , drpNetworks
  , drpKernel

  , snpId
  , snpName
  , snpDist
  , snpSlug
  , snpPublic
  , snpRegions

  , bckId
  , bckName
  , bckDist
  , bckSlug
  , bckPublic
  , bckRegions

  , dcoName
  , dcoRegion
  , dcoSize
  , dcoImage
  , dcoSSHKeys
  , dcoBackups
  , dcoIpv6
  , dcoPrivateNetworking
  , dcoUserData

  ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), (.=), object, Value)
import Data.Maybe (catMaybes)
import Control.Lens hiding ((.=), Action)
import Control.Applicative

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError, Error)

import Net.DigitalOcean.Request (get, post, delete)
import Net.DigitalOcean.Config (Config)
import Net.DigitalOcean.Regions (Region)
import Net.DigitalOcean.Images (Image)
import Net.DigitalOcean.Actions (Action)

data Network = Network
               { _netProto :: !T.Text
               , _netIpAddr :: !T.Text
               , _netNetmask :: !T.Text
               , _netGateway :: !T.Text
               , _netType :: !T.Text
               } deriving (Show, Eq)
makeLenses ''Network

instance FromJSON [Network] where
  parseJSON (Object obj) = sequence . concat $ mapM parseEntry (HM.toList obj)
    where parseEntry (ty, Array vals) = map (parseNetwork ty) (V.toList vals)
          parseEntry _ = error "network list must be arr"
          parseNetwork ty (Object n) =
            Network ty <$>
            n .: "ip_address" <*>
            n .: "netmask" <*>
            n .: "gateway" <*>
            n .: "type"
          parseNetwork _ _ = error "network must be object"
  parseJSON _ = error "network must be object"

data Kernel = Kernel
              { _krnId :: !Int
              , _krnName :: !T.Text
              , _krnVersion :: !T.Text
              } deriving (Show, Eq)
makeLenses ''Kernel

instance FromJSON Kernel where
  parseJSON (Object x) = Kernel <$>
                         x .: "id" <*>
                         x .: "name" <*>
                         x .: "version"
  parseJSON _ = error "kernel must be object"

data Droplet = Droplet
               { _drpId :: !Int
               , _drpName :: !T.Text
               , _drpMemory :: !Int
               , _drpVCPUs :: !Int
               , _drpDisk :: !Int
               , _drpLocked :: !Bool
               , _drpCreatedAt :: !T.Text
               , _drpStatus :: !T.Text
               , _drpBackupIds :: ![T.Text]
               , _drpSnapshotIds :: ![T.Text]
               , _drpFeatures :: ![T.Text]
               , _drpRegion :: Maybe Region
               , _drpImage :: Maybe Image
               , _drpNetworks :: [Network]
               , _drpKernel :: Maybe Kernel
               } deriving (Show, Eq)
makeLenses ''Droplet

instance FromJSON Droplet where
  parseJSON (Object x) = Droplet <$>
                         x .: "id" <*>
                         x .: "name" <*>
                         x .: "memory" <*>
                         x .: "vcpus" <*>
                         x .: "disk" <*>
                         x .: "locked" <*>
                         x .: "created_at" <*>
                         x .: "status" <*>
                         x .: "backup_ids" <*>
                         x .: "snapshot_ids" <*>
                         x .: "features" <*>
                         x .: "region" <*>
                         x .: "image" <*>
                         x .: "networks" <*>
                         x .: "kernel"
  parseJSON _ = error "droplet must be object"

data Snapshot = Snapshot
                { _snpId :: !Int
                , _snpName :: !T.Text
                , _snpDist :: !T.Text
                , _snpSlug :: Maybe T.Text
                , _snpPublic :: !Bool
                , _snpRegions :: ![T.Text]
                } deriving (Show, Eq)
makeLenses ''Snapshot

instance FromJSON Snapshot where
  parseJSON (Object x) = Snapshot <$>
                         x .: "id" <*>
                         x .: "name" <*>
                         x .: "dist" <*>
                         x .: "slug" <*>
                         x .: "public" <*>
                         x .: "regions"
  parseJSON _ = error "snapshot must be object"

data Backup = Backup
                { _bckId :: !Int
                , _bckName :: !T.Text
                , _bckDist :: !T.Text
                , _bckSlug :: Maybe T.Text
                , _bckPublic :: !Bool
                , _bckRegions :: ![T.Text]
                } deriving (Show, Eq)
makeLenses ''Backup

instance FromJSON Backup where
  parseJSON (Object x) = Backup <$>
                         x .: "id" <*>
                         x .: "name" <*>
                         x .: "dist" <*>
                         x .: "slug" <*>
                         x .: "public" <*>
                         x .: "regions"
  parseJSON _ = error "backup must be object"

data DropletCreationOpts = DropletCreationOpts
                           { _dcoName :: !T.Text
                           , _dcoRegion :: !T.Text
                           , _dcoSize :: !T.Text
                           , _dcoImage :: !T.Text
                           , _dcoSSHKeys :: Maybe [T.Text]
                           , _dcoBackups :: !Bool
                           , _dcoIpv6 :: !Bool
                           , _dcoPrivateNetworking :: !Bool
                           , _dcoUserData :: Maybe T.Text
                           } deriving (Show, Eq)
makeLenses ''DropletCreationOpts

instance ToJSON DropletCreationOpts where
  toJSON d = object (base ++ catMaybes opts)
    where base = [ "name" .= (d ^. dcoName)
                 , "region" .= (d ^. dcoRegion)
                 , "size" .= (d ^. dcoSize)
                 , "image" .= (d ^. dcoImage)
                 , "backups" .= (d ^. dcoBackups)
                 , "ipv6" .= (d ^. dcoIpv6)
                 , "private_networking" .= (d ^. dcoPrivateNetworking)]
          opts = [ fmap ("user_data" .=) (d ^. dcoUserData)
                 , fmap ("ssh_keys" .=) (d ^. dcoSSHKeys)
                 ]

-- | Creates a basic Droplet creation options object with ipv6 and private
-- networking enabled, and without backups, user data or ssh keys.
--
-- Can and should be further modified before passing to 'createDroplet'
defaultDCO :: T.Text -> T.Text -> T.Text -> T.Text -> DropletCreationOpts
defaultDCO n r s i = DropletCreationOpts n r s i Nothing False True True Nothing

dropletsEndpoint :: String
dropletsEndpoint = "/v2/droplets/"

dropletEndpoint :: T.Text -> String
dropletEndpoint = (++) dropletsEndpoint . T.unpack

createDroplet :: (Error e, MonadError e m, MonadIO m) =>
                 DropletCreationOpts -> Config -> m Droplet
createDroplet = post dropletsEndpoint "droplet"

getDroplet :: (Error e, MonadError e m, MonadIO m) =>
              T.Text -> Config -> m Droplet
getDroplet n = get (dropletEndpoint n) "droplet"

getDroplets ::(Error e, MonadError e m, MonadIO m) =>
              Config -> m [Droplet]
getDroplets = get dropletsEndpoint "droplets"

getDropletKernels :: (Error e, MonadError e m, MonadIO m) =>
                     T.Text -> Config -> m [Kernel]
getDropletKernels n = get (dropletEndpoint n ++ "/kernels") "kernels"

getDropletSnapshots :: (Error e, MonadError e m, MonadIO m) =>
                       T.Text -> Config -> m [Snapshot]
getDropletSnapshots n = get (dropletEndpoint n ++ "/snapshots") "snapshots"

getDropletBackups :: (Error e, MonadError e m, MonadIO m) =>
                     T.Text -> Config -> m [Backup]
getDropletBackups n = get (dropletEndpoint n ++ "/backups") "backups"

getDropletActions :: (Error e, MonadError e m, MonadIO m) =>
                     T.Text -> Config -> m [Action]
getDropletActions n = get (dropletEndpoint n ++ "/actions") "actions"

deleteDroplet :: (Error e, MonadError e m, MonadIO m) =>
                 T.Text -> Config -> m ()
deleteDroplet = delete . dropletEndpoint

-- TODO: Get droplet upgrades returns a list...

-- | The various actions that can be performed on a droplet
--
-- <https://developers.digitalocean.com/#droplet-actions DO documentation>
data DropletAction
  = DisableBackups
  | Reboot
  | PowerCycle
  | Shutdown
  | PowerOff
  | PowerOn
  | Restore
  | PasswordReset
  | Resize !T.Text
  | Rebuild !T.Text
  | Rename !T.Text
  | ChangeKernel !Int
  | EnableIpv6
  | EnablePrivateNetworking
  | SnapshotDrp !T.Text
  | Upgrade

mType :: T.Text -> [(T.Text, Value)] -> Value
mType t rst = toJSON . HM.fromList $ ("type", toJSON t):rst

instance ToJSON DropletAction where
  toJSON DisableBackups = mType "disable_backups" []
  toJSON Reboot = mType "reboot" []
  toJSON PowerCycle = mType "power_cycle" []
  toJSON Shutdown = mType "shutdown" []
  toJSON PowerOff = mType "power_off" []
  toJSON PowerOn = mType "power_on" []
  toJSON Restore = mType "restore" []
  toJSON PasswordReset = mType "password_reset" []
  toJSON (Resize s) = mType "resize" [("size", toJSON s)]
  toJSON (Rebuild i) = mType "rebuild" [("image", toJSON i)]
  toJSON (Rename n) = mType "rename" [("name", toJSON n)]
  toJSON (ChangeKernel i) = mType "change_kernel" [("kernel", toJSON i)]
  toJSON EnableIpv6 = mType "enable_ipv6" []
  toJSON EnablePrivateNetworking = mType "enable_private_networking" []
  toJSON (SnapshotDrp n) = mType "snapshot" [("snapshot", toJSON n)]
  toJSON Upgrade = mType "migrate_droplet" []

-- | Perform an action on the droplet with the given ID
performDropletAction :: (Error e, MonadError e m, MonadIO m) =>
                        T.Text -> DropletAction -> Config -> m Action
performDropletAction n = post (dropletEndpoint n ++ "/actions") "action"
