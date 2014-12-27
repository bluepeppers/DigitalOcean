module Main where

import qualified Net.DigitalOcean as DO
import qualified Data.Text as T
import Test.Hspec

import Control.Lens
import Data.Char (isSpace)

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

getAuthToken :: IO String
getAuthToken = fmap rstrip $ readFile "./auth_token.txt"

getConfig :: IO DO.Config
getConfig = fmap (DO.Config . T.pack) getAuthToken

main :: IO ()
main = do
  c <- getConfig
  putStrLn $ show c
  hspec $ do
    describe "SSHKeys.getSSHKeys" $ do
      it "returns (at least) a key named milo" $ do
        keys <- DO.getSSHKeys c
        keys `shouldSatisfy` any (\k -> k ^. DO.keyName == "Milo")
