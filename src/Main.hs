{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (lines)
import Shelly hiding (fromText)
import System.Environment
import Data.Maybe
import Data.Text (pack, unpack, intercalate, append, lines)
import Text.Regex.Applicative
import Data.Char

-- XXX TODO Maybe return from (=~) is all well and good until `read` fails on an int...
--          How best to handle this? A better way to invoke than =~ ?
type TorrentId = Integer
type Faulty = Bool
data DonePct = DoneNA
             | DonePct Integer deriving (Eq, Show)
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
-- TODO move to own module and the remove sl prefix and expot
data StatusLine = StatusLine { slId :: TorrentId
                             , slFaulty :: Faulty
                             , slDone :: DonePct
                             , slHave :: Have
                             , slEta :: ETA
                             , slUp :: Up
                             , slDown :: Down
                             , slRatio :: Ratio
                             , slStatus :: Status
                             , slName :: Name
                             } deriving Show

statusLine = StatusLine <$  spaces
                        <*> torrentId
                        <*> faulty <* spaces
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
          torrentId = intfmt
          faulty = True <$ string "*"
               <|> pure False
          donePct = DoneNA <$ string "n/a"
                <|> DonePct <$> (liftA read $ many $ psym isDigit) <* string "%"
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

-- TODO replace shelly with shell-conduit, once I get that working.
xm_bin = "transmission-remote"

xm args = do
    run_ xm_bin args

xmo args = do
    let op:ids = args
    run_ xm_bin ["-t" `append` intercalate "," ids, op]

xmcheck args = do
    ids <- getFinishedIds
    run_ xm_bin ["-t" `append` intercalate "," ids, "-v"]

xmf args = do
    ids <- getFinishedIds
    run_ xm_bin ["-t" `append` intercalate "," ids, "-l"]

-- XXX TODO handle exception on parse returning Nothing
-- XXX skip first and last line better (just handle and skip Nothing?)
getFinishedIds = do
    tors <- run "transmission-remote" ["-l"]
    let _:skipFooter = (reverse . lines) tors
    let _:skipHeader = reverse skipFooter
    let fin = filter isFinished $ map parse skipHeader
    return $ map (pack . show . slId) fin
    where parse = (fromJust . (flip (=~) statusLine) . unpack)
          isFinished StatusLine{..} = not slFaulty && slDone == DonePct 100
{- TODO
xmclean() {
    local XM_TORS="/home/keb/.config/transmission-daemon/torrents"
    local XM_TORS_BK="/home/keb/Downloads/_tors"
    rsync -rP ${XM_TORS}/ ${XM_TORS_BK} || return
    local ids=`_xm_get_finished_and_idle_ids`
    test -z "$ids" || transmission-remote -t $ids -r
}
-}

-- TODO replace the lookup with template-haskell or something
calls = [ ("xm", xm)
        , ("xmo", xmo)
        , ("xmf", xmf)
        , ("xmcheck", xmcheck)
        ]

-- TODO: offer to create or intelligently know when to create multi-call links
main = do
    name <- getProgName
    args <- getArgs
    let call = fromJust $ lookup name calls
    shelly $ call (map pack args)
