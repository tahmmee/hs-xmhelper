{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Turtle
import Prelude hiding (lines, unlines, putStr, putStrLn, readFile)
import System.Environment
import Data.Maybe
import Data.Text (pack, unpack, intercalate, lines, unlines, strip, Text)
import Data.Text.IO (putStr, putStrLn)
--import Data.Foldable (toList)
import qualified Control.Foldl as Foldl

import StatusParser
import Text.Regex.Applicative

import TransmissionConfig
import Control.Monad

-- TODO remove this convenience func i added at the start of turtle porting
run :: Text -> [Text] -> Shell Text
run cmd args = inshell (intercalate " " $ [cmd] <> args) empty

xm :: [Text] -> Shell Text
xm args = run "transmission-remote" args

xmOn :: Shell Integer -> [Text] -> Shell Text
xmOn ids args = do
    idList <- fold ids Foldl.list
    case idList of
        [] -> "Nothing to do; no torrent ids given."
        _  -> do
             let idts = map (pack . show) idList
             xm $ ["-t" <> intercalate "," idts] <> args

xmo args = do
    let op:ids = args
    xmOn (select $ map (read . unpack) ids) [op]

xmcheck args = do
    xmOn getFinishedIds ["-v"]

xmf args = do
    xmOn getFinishedIds ["-l"]

xmclean args = do
    TransmissionConfig{..} <- liftIO getConfig
    src <- liftIO getConfigDir
    let dest = downloadDir <> "/_torrents"
    ec <- view $ shell ("rsync -rP " <> src <> " /torrents/ " <> dest) empty
    -- TODO check rsync is in path
    -- TODO Guard here to make sure the above executed correctly
    -- (NB currently failure exits uncleanly because of attr preservation)
    xmOn getFinishedIds ["-r"]

-- TODO handle exception on parse returning Nothing
getFinishedIds :: Shell Integer
getFinishedIds = do
    lines <- parse <$> xm ["-l"]
    --lines <- parse <$> inshell "cat sample" empty
    case isFinished lines of
        True -> tid <$> pure lines
        False -> empty
    --TODO write like this--tid <$> isFinished <$> parse <$> inshell "cat sample" empty

    where
          body = tail . reverse . tail . reverse -- strip first and last line
          -- XXX fromJust should never fail since failed parse returns its own data constructor. still a better way to write in applicative context?
          parse = fromJust . (=~ statusLine) . unpack
          isFinished StatusLine{..} = not faulty && done == DonePct 100
          --isFinished x@StatusLine{..}
          --  | not faulty && done == DonePct 100 = True
          --  | otherwise = False
          isFinished _ = False

xmtest args = do
    -- test whatever here
    --return "test"
    ids <- getFinishedIds
    return $ pack . show $ ids
    

-- TODO replace the lookup with template-haskell or something
calls :: [(Text, [Text] -> Shell Text)]
calls = [ ("xm"     , xm     ) -- shortcut for "transmission-remote"
        , ("xmo"    , xmo    ) -- Operate on listed ids. e.g. "xmo -v `seq 2 4`"
        , ("xmf"    , xmf    ) -- list Finished status lines (100% and not faulty)
        , ("xmcheck", xmcheck) -- verify finished torrents
        , ("xmclean", xmclean) -- use rsync to backup torrent files, then remove idle and finished torrents
        , ("xmtest" , xmtest) -- XXX test
        ]

-- TODO: offer to create or intelligently know when to create multi-call links
main :: IO ()
main = do
    name <- getProgName >>= return . pack
    args <- getArgs >>= return . map pack
    let call = fromJust $ lookup name calls
    sh (call args >>= liftIO . putStrLn)
