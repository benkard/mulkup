{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Mulkup.Bupstash (BupItem(..), Bupstash (..), bupPut, bupGc, bupList, bupRemove, BupFilter (..), runBupstash, bupItemUTCTime) where

import Mulkup.Config (MulkupConfig (..))
import Mulkup.Prelude hiding (put)
import Optics
import Polysemy
import Polysemy.Reader (Reader, asks)
import Turtle hiding (err, x)
import Data.Aeson (FromJSON, eitherDecode)
import Polysemy.Error (Error, throw)
import Data.Text (pack, unpack)
import Mulkup.Logging
import Colog.Polysemy (Log)
import Colog (Message)
import Data.Time
import Data.Time.Clock.POSIX

-- * API

data BupItem = BupItem
  { id :: Text,
    unix_timestamp_millis :: Integer,
    tags :: Map String String
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data BupFilter = BupFilter
  { labels :: [(Text, Text)],
    minimumAge :: Maybe Text
  }

makeFieldLabelsNoPrefix ''BupFilter

data Bupstash m a where
  BupGc :: Bupstash m ()
  BupPut :: Text -> [Text] -> [(Text, Text)] -> Bupstash m ()
  BupList :: BupFilter -> Bupstash m [BupItem]
  BupRemove :: [Text] -> Bupstash m ()

makeSem ''Bupstash

-- * Smart Accessors

bupItemUTCTime :: BupItem -> UTCTime
bupItemUTCTime item =
  posixSecondsToUTCTime $
    secondsToNominalDiffTime $
      fromInteger (item ^. #unix_timestamp_millis) / 1000

-- * Implementation

-- | Runs a 'Bupstash' using the “bupstash” CLI command.
runBupstash :: (Member (Error Text) r, Member (Log Message) r, Member (Embed IO) r, Member (Reader MulkupConfig) r) => Sem (Bupstash ': r) a -> Sem r a
runBupstash = interpret \case
  BupGc ->
    procs "bupstash" ["gc"] empty

  BupPut baseDir exclusions labels -> do
    host <- getHost
    procs "bupstash" (["put", "--xattrs"] ++ map exclusionArg exclusions ++ map labelArg labels ++ [labelArg ("host", host)] ++ [baseDir]) empty

  BupList bupFilter -> do
    host <- getHost
    out <- strict $ inproc "bupstash" (["list", "--format=jsonl1"] ++ filterArgs host bupFilter) empty
    let parsedItems = map parseItem (patchLines (lines out))
    forM parsedItems \case
      Left err -> do
        let errtext = pack err
        logError errtext
        throw errtext
      Right x ->
        return x

  BupRemove ids -> do
    procs "bupstash" ["rm", "--ids-from-stdin"] (select (map unsafeTextToLine ids))

  where
    getHost :: Member (Reader MulkupConfig) r => Sem r Text
    getHost =
      asks @MulkupConfig (^. #host)

-- | Fixes up the buggy two-line output that Buptash produces in
-- jsonl1 output mode.
--
-- See: https://github.com/andrewchambers/bupstash/pull/241
patchLines :: [Text] -> [Text]
patchLines = concatMap patchLine
  where
    patchLine :: Text -> [Text]
    patchLine line
      | line == "}" =
        []
      | length (filter (== '}') $ unpack line) < length (filter (== '{') $ unpack line) =
        [line <> "}"]
      | otherwise =
        [line]

labelArg :: (Text, Text) -> Text
labelArg (key, value) = key <> "=" <> value

exclusionArg :: Text -> Text
exclusionArg = ("--exclude=" <>)

filterArgs :: Text -> BupFilter -> [Text]
filterArgs host (BupFilter labels minimumAge) =
  [labelArg ("host", host)] ++
    concatMap (\label -> ["and", labelArg label]) labels ++
    concatMap (\x -> ["and", "older-than", x]) minimumAge

parseItem :: Text -> Either String BupItem
parseItem = eitherDecode . encodeUtf8
