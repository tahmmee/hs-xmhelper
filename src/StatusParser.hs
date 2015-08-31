module StatusParser ( StatusLine(..)
                    , statusLine
                    -- TODO do i need type aliases exported?
                    --, TorrentId
                    --, Faulty
                    , DonePct(..)
                    , ETA(..)
                    --, Up
                    --, Down
                    , Ratio(..)
                    , Status(..)
                    --, Name
) where

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
data StatusLine = StatusLine { tid :: TorrentId
                             , faulty :: Faulty
                             , done :: DonePct
                             , have :: Have
                             , eta :: ETA
                             , up :: Up
                             , down :: Down
                             , ratio :: Ratio
                             , status :: Status
                             , name :: Name
                             }
                | UnparseableStatusLine
                deriving Show

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
         <|> UnparseableStatusLine <$ many anySym
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
