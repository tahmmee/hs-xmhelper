{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (lines, unlines, putStr, readFile)
import Shelly hiding (fromText)
import System.Environment
import Data.Maybe
import Data.Text (pack, unpack, intercalate, append, lines, unlines, strip)
import Data.Text.IO (putStr)

import StatusParser
import Text.Regex.Applicative

import TransmissionConfig
import Control.Monad


-- TODO replace shelly with shell-conduit, once I get that working.
xm args = run "transmission-remote" args

xmOn ids args = xm $ ["-t" `append` intercalate "," (map (pack . show) ids)] ++ args

xmo args = do
    let op:ids = args
    xmOn ids [op]

xmcheck args = do
    ids <- getFinishedIds
    xmOn ids ["-v"]

xmf args = do
    ids <- getFinishedIds
    xmOn ids ["-l"]

-- XXX TODO use Aeson to parse settings.json for the paths
torrentsDir = "~/.config/transmission-daemon/torrents"
torrentsDestDir = "~/Downloads/_tors"
xmclean args = do
    -- XXX expanding vars this way is ugly
    src <- expand torrentsDir
    dest <- expand torrentsDestDir
    -- TODO check rsync is in path
    out <- run "rsync" ["-rP", (src `append` "/"), dest]
    liftIO $ putStr out
    -- TODO Guard here to make sure the above executed correctly
    ids <- getFinishedIds
    xmOn ids ["-l"]
    where expand var = run "bash" ["-c" , "echo " `append` var] >>= return . strip

-- XXX TODO handle exception on parse returning Nothing
-- XXX skip first and last line better (just handle and skip Nothing?)
getFinishedIds = do
    tors <- xm ["-l"]
    return [ tid parsed | line <- body tors
           , let parsed = parse line
           , isFinished parsed ]
    where parse = fromJust . (flip (=~) statusLine) . unpack
          body = tail . reverse . tail . reverse . lines  -- strip first and last line
          isFinished StatusLine{..} = not faulty && done == DonePct 100

whoAmI = lookupEnv "USER" >>= return . pack . fromJust
getHomeDir = lookupEnv "HOME" >>= return . pack . fromJust

xmtest args = do
    --liftIO $ forM [whoAmI, getHomeDir] $ id >=> (putStrLn . unpack)
    out <- liftIO $ forM [whoAmI, getHomeDir] id
    return $ unlines out
    

-- TODO replace the lookup with template-haskell or something
calls = [ ("xm",      xm     ) -- shortcut for "transmission-remote"
        , ("xmo",     xmo    ) -- Operate on listed ids. e.g. "xmo -v `seq 2 4`"
        , ("xmf",     xmf    ) -- list Finished status lines (100% and not faulty)
        , ("xmcheck", xmcheck) -- verify finished torrents
        , ("xmclean", xmclean) -- use rsync to backup torrent files, then remove idle and finished torrents
        , ("xmtest", xmtest) -- XXX test
        ]

-- TODO: offer to create or intelligently know when to create multi-call links
main = do
    name <- getProgName
    args <- getArgs
    let call = fromJust $ lookup name calls
    out <- shelly $ silently $ call (map pack args)
    putStr out
