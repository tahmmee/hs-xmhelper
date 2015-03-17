{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (lines, putStr)
import Shelly hiding (fromText)
import System.Environment
import Data.Maybe
import Data.Text (pack, unpack, intercalate, append, lines)
import Data.Text.IO (putStr)
import StatusParser
import Text.Regex.Applicative


-- TODO replace shelly with shell-conduit, once I get that working.
xm_bin = "transmission-remote"

xm args = run xm_bin args

xmo args = do
    let op:ids = args
    xm ["-t" `append` intercalate "," ids, op]

xmcheck args = do
    ids <- getFinishedIds
    xm ["-t" `append` intercalate "," ids, "-v"]

xmf args = do
    ids <- getFinishedIds
    xm ["-t" `append` intercalate "," ids, "-l"]

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
{- TODO use Aeson to parse settings.json for the paths
xmclean() {
    local XM_TORS="/home/keb/.config/transmission-daemon/torrents"
    local XM_TORS_BK="/home/keb/Downloads/_tors"
    rsync -rP ${XM_TORS}/ ${XM_TORS_BK} || return
    local ids=`_xm_get_finished_and_idle_ids`
    test -z "$ids" || transmission-remote -t $ids -r
}
-}

-- TODO replace the lookup with template-haskell or something
calls = [ ("xm", xm) -- shortcut for "transmission-remote"
        , ("xmo", xmo) -- Operate on listed ids. e.g. "xmo -v `seq 2 4`"
        , ("xmf", xmf) -- list Finished status lines (100% and not faulty)
        , ("xmcheck", xmcheck) -- verify finished torrents
        --, ("xmclean", xmverify) -- use rsync to backup torrent files, then remove idle and finished torrents
        ]

-- TODO: offer to create or intelligently know when to create multi-call links
main = do
    name <- getProgName
    args <- getArgs
    let call = fromJust $ lookup name calls
    out <- shelly $ silently $ call (map pack args)
    putStr out
