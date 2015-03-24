{-# LANGUAGE OverloadedStrings #-}
module TransmissionConfig ( TransmissionConfig(..)
                          , getConfig
) where

import Data.Aeson
import Control.Monad
import Data.Text
import Data.ByteString.Lazy (readFile)
import Prelude hiding (unlines, readFile)
import Data.Maybe

data TransmissionConfig = TransmissionConfig { downloadDir :: Text
                                             , incompleteDir :: Text
                                             } deriving Show

instance FromJSON TransmissionConfig where
    parseJSON (Object v) = TransmissionConfig <$> v .: "download-dir"
                                              <*> v .: "incomplete-dir"
    parseJSON _          = mzero

getConfig :: IO TransmissionConfig
getConfig = do
    input <- readFile "settings.json"
    return $ fromJust $ decode input
