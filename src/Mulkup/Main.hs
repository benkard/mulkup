{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Mulkup.Main where

import Colog (Message, richMessageAction, simpleMessageAction)
import Colog.Polysemy.Effect (Log, runLogAction)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Time
import Mulkup.Bupstash
import Mulkup.Config
import Mulkup.Flags
import Mulkup.Logging
import Mulkup.Prelude
import Optics
import Options.Applicative
  ( execParser,
    fullDesc,
    helper,
    info,
  )
import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.Final
import Polysemy.Reader (Reader, asks, runReader)

main :: IO ()
main = do
  flags <- execParser $ info (flagParser <**> helper) fullDesc
  let messageAction =
        if verbose flags
          then richMessageAction
          else simpleMessageAction

  config <- readConfig "./config.dhall"

  result <-
    main'
      & runBupstash
      & runLogAction @IO messageAction
      & runReader (config :: MulkupConfig)
      & errorToIOFinal @Text
      & embedToFinal @IO
      & runFinal @IO

  case result of
    Left err -> do
      error err
    Right () ->
      return ()

main' :: (Member (Log Message) r, Member (Reader MulkupConfig) r, Member Bupstash r) => Sem r ()
main' = do
  stashes <- asks @MulkupConfig (^. #stashes)
  forM_ stashes $ \stash -> do
    let labels = [("name", stash ^. #name)]

    currentItems <- bupList (BupFilter labels Nothing)

    let tiers =
          [ (utctHour . bupItemUTCTime, #hourly),
            (utctJulianDay . bupItemUTCTime, #daily),
            (utctWeek . bupItemUTCTime, #weekly),
            (utctMonth . bupItemUTCTime, #monthly)
          ]

    let keepIds =
          Set.unions $
            map
              ( \(discriminator, cfgLens) ->
                  let tierCfg = stash ^. #tiers ^. cfgLens
                   in tierKeepIds discriminator (tierCfg ^. #keep) currentItems
              )
              tiers

    let currentIds = Set.fromList (map (^. #id) currentItems)
    let rmIds = Set.difference currentIds keepIds

    logInfo (show labels <> " Keeping: " <> show (Set.toList keepIds))
    logInfo (show labels <> " Removing: " <> show (Set.toList rmIds))
    bupRemove (Set.toList rmIds)

    logInfo (show labels <> " Creating backup.")
    bupPut
      (stash ^. #baseDir)
      (stash ^. #exclusions)
      labels

tierKeepIds :: (BupItem -> Integer) -> Natural -> [BupItem] -> Set Text
tierKeepIds discriminator keep items =
  fromList $
    take (fromIntegral keep) $
      map (^. #id) $
        reverse $
          sortWith bupItemUTCTime $
            map (head . NonEmpty.sortWith bupItemUTCTime) $
              elems $
                (groupBy discriminator items :: HashMap Integer (NonEmpty BupItem))

utctHour :: UTCTime -> Integer
utctHour (UTCTime (ModifiedJulianDay julianDay) tdiff) = (julianDay * 24) + (diffTimeToPicoseconds tdiff `div` (10 ^ (12 :: Integer)) `div` 3600)

utctJulianDay :: UTCTime -> Integer
utctJulianDay (UTCTime (ModifiedJulianDay julianDay) _) = julianDay

utctWeek :: UTCTime -> Integer
utctWeek (UTCTime day _) =
  julianDay + fromIntegral (dayOfWeekDiff (dayOfWeek day) Sunday)
  where
    (ModifiedJulianDay julianDay) = day
    dayOfWeekDiff a b = mod (fromEnum a - fromEnum b) 7

utctMonth :: UTCTime -> Integer
utctMonth (UTCTime day _) = fromIntegral m
  where
    (_, m, _) = toGregorian day
