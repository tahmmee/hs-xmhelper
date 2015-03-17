{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (concat)
import Shelly hiding (fromText)
import System.Environment
import Data.Maybe
import Data.Text (pack, unpack, intercalate, append)
import Control.Monad hiding (join)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Text.Regex.Applicative
import Data.Char

data TorrentId = TorrentId Integer | FaultyTorrentId Integer deriving Show
data DonePct = DonePct Integer deriving Show
data Have = NoHave
          | Have { haveAmount :: Float
                 , haveUnits  :: String } deriving Show
data ETA = Done
         | Unknown
         | ETA { etaAmount :: Integer
               , etaUnits  :: String }
         | OtherETA String deriving Show
type Up = Float
type Down = Float
data Ratio = Ratio Float
           | NoRatio deriving Show
data Status = Bidirectional
            | Idle
            | Seeding
            | Stopped
            | Queued
            | Verifying
            | WillVerify
            | OtherStatus String deriving Show
type Name = String
data StatusLine = StatusLine TorrentId DonePct Have ETA Up Down Ratio Status Name deriving Show

statusLine = StatusLine <$  spaces
                        <*> torrentId <* spaces
                        <*> donePct <* spaces
                        <*> have <* spaces
                        <*> eta <* spaces
                        <*> up <* spaces
                        <*> down <* spaces
                        <*> ratio <* spaces
                        <*> status <* spaces
                        <*> name <* spaces
    where spaces = many (psym isSpace)
          intfmt = liftA read $ many $ psym isDigit
          floatfmt = liftA read $ many $ psym (\x -> isDigit x || (== '.') x)
          torrentId = FaultyTorrentId <$> intfmt <* string "*"
                  <|> TorrentId       <$> intfmt
          donePct =  DonePct <$> (liftA read $ many $ psym isDigit) <* string "%"
          have = NoHave <$ string "None"
             <|> Have <$> floatfmt <* psym isSpace <*> many (psym isAlpha)
          eta = Done <$ string "Done"
            <|> Unknown <$ string "Unknown"
            <|> ETA <$> intfmt <* psym isSpace <*> many (psym isAlpha)
            <|> OtherETA <$> many (psym isAlpha)
          up = floatfmt
          down = floatfmt
          ratio = NoRatio <$ string "None"
              <|> Ratio <$> floatfmt
          status = Bidirectional <$ string "Up & Down"
               <|> Idle <$ string "Idle"
               <|> Seeding <$ string "Seeding"
               <|> Stopped <$ string "Stopped"
               <|> Queued <$ string "Queued"
               <|> Verifying <$ string "Verifying"
               <|> WillVerify <$ string "Will Verify"
               <|> OtherStatus <$> many (psym isAlpha)
          name = many anySym

xm args = do
    run_ "transmission-remote" args

xmo args = do
    let op:ids = args
    run_ "transmission-remote" ["-t" `append` intercalate "," ids, op]

xmcheck args = do
    getFinishedIds

getFinishedIds = do
    tors <- run "transmission-remote" ["-l"]
    liftIO $ print tors
{-
_xm_get_finished_ids() {
    # XXX trailing , is okay
    # FIXME string should be null if none
    transmission-remote -l | egrep '[0-9]+ +100%' | awk '{print $1}' | tr '\n' ','
}
_xm_get_finished_and_idle_ids() {
    # XXX trailing , is okay
    # FIXME string should be null if none
    transmission-remote -l | egrep '[0-9]+ +100%.*(Seeding|Idle)' | awk '{print $1}' | tr '\n' ','
}
xmf() {
    transmission-remote -t `_xm_get_finished_ids` -l
}
xmclean() {
    local XM_TORS="/home/keb/.config/transmission-daemon/torrents"
    local XM_TORS_BK="/home/keb/Downloads/_tors"
    rsync -rP ${XM_TORS}/ ${XM_TORS_BK} || return
    local ids=`_xm_get_finished_and_idle_ids`
    test -z "$ids" || transmission-remote -t $ids -r
}
xmcheck() {
    transmission-remote -t `_xm_get_finished_ids` -v
}

-}

-- TODO replace the lookup with template-haskell or something
calls = [ ("xm", xm)
        , ("xmo", xmo)
        , ("xmcheck", xmcheck)
        ]

main = do
    text <- readFile "sample.txt"
    forM_ (lines text) $ \line -> do
        print $ line =~ statusLine
--main = do
--    name <- getProgName
--    args <- getArgs
--    let call = fromJust $ lookup name calls
--    shelly . verbosely $ call (map pack args)
