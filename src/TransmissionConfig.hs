{-# LANGUAGE OverloadedStrings #-}
module TransmissionConfig ( TransmissionConfig(..)
                          , getConfig
                          , getConfigDir
) where

import Data.Aeson
import Control.Monad
import Data.Text
import Data.ByteString.Lazy (readFile)
import Prelude hiding (unlines, readFile)
import Data.Maybe
import System.Environment

data TransmissionConfig = TransmissionConfig { downloadDir :: Text
                                             , incompleteDir :: Text
                                             } deriving Show

instance FromJSON TransmissionConfig where
    parseJSON (Object v) = TransmissionConfig <$> v .: "download-dir"
                                              <*> v .: "incomplete-dir"
    parseJSON _          = mzero

whoAmI = lookupEnv "USER" >>= return . pack . fromJust
getHomeDir = lookupEnv "HOME" >>= return . pack . fromJust

getConfigDir = do
    uid <- whoAmI
    case uid of
        "transmission" -> return "/var/lib/transmission/config"
        _ -> getHomeDir >>= return . (flip append "/.config/transmission-daemon")

getConfigPath = getConfigDir >>= return . (flip append "/settings.json")

getConfig :: IO TransmissionConfig
getConfig = do
    path <- getConfigPath
    input <- readFile $ unpack path
    return $ fromJust $ decode input
