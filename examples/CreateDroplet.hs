module Main where

import qualified Data.Text as T
import qualified Net.DigitalOcean as DO
import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Lens

main :: IO ()
main = do
  auth <- fmap head getArgs

  putStrLn . show . getSSHKey . DO.Config . T.pack $ auth

getSSHKey :: T.Text -> DO.Config -> IO DO.SSHKey
getSSHKey keyName c =
  DO.getSSHKeys c >>= \keys ->
  case find (\k -> k ^. DO.keyName == keyName) keys of
    Just k -> return k
    Nothing -> error "No key found"

createDroplet c =
  let region = "nyc3"
      sshKeyName = "Milo"
      image = "ubuntu-14-04-x64"
      size = "512mb"
      dropletName = "test"

  sshKey <- getSSHKey sshKeyName c
  let dco = (DO.defaultDCO dropletName region size image)
            & DO.dcoSSHKeys ?~ [sshKey ^. DO.keyFingerprint]
  DO.createDroplet dco c
