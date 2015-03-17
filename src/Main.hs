{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (lines)
import Shelly hiding (fromText)
import System.Environment
import Data.Maybe
import Data.Text (pack, unpack, intercalate, append, lines)
import StatusParser
import Text.Regex.Applicative


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
    return $ map (pack . show . tid) fin
    where parse = (fromJust . (flip (=~) statusLine) . unpack)
          isFinished StatusLine{..} = not faulty && done == DonePct 100
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
