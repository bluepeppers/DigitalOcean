module Main where

import qualified Data.Text as T
import qualified Net.DigitalOcean as DO
import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Lens

main :: IO ()
main = do
  auth <- fmap head getArgs

  let c = DO.Config . T.pack $ auth
  keys <- DO.getSSHKeys c
  printf "Got %d keys" (length keys)
  let printKey k = printf "Key %s: %s"
                     (show $ k ^. DO.keyName)
                     (show $ k ^. DO.keyFingerprint)
  mapM_ printKey keys
