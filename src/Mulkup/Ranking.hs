{-# LANGUAGE UndecidableInstances #-}

module Mulkup.Ranking (rankBupItems) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Time
import Mulkup.Bupstash
import Mulkup.Config
import Mulkup.Prelude
import Optics

rankBupItems :: TierConfigs -> [BupItem] -> (Set Text, Set Text)
rankBupItems tierConfigs currentItems =
  (keepIds, rmIds)
  where
    tiers =
      [ (utctHour . bupItemUTCTime, #hourly),
        (utctJulianDay . bupItemUTCTime, #daily),
        (utctWeek . bupItemUTCTime, #weekly),
        (utctMonth . bupItemUTCTime, #monthly)
      ]

    keepIds =
      Set.unions $
        map
          ( \(discriminator, cfgLens) ->
              let tierCfg = tierConfigs ^. cfgLens
               in tierKeepIds discriminator (tierCfg ^. #keep) currentItems
          )
          tiers

    currentIds = Set.fromList (map (^. #id) currentItems)
    rmIds = Set.difference currentIds keepIds

tierKeepIds :: (BupItem -> Integer) -> Natural -> [BupItem] -> Set Text
tierKeepIds discriminator keep items =
  fromList $
    take (fromIntegral keep) $
      map (^. #id) $
        reverse $
          sortWith bupItemUTCTime $
            map (head . NonEmpty.sortWith bupItemUTCTime) $
              elems
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

utctMonth :: UTCTime -> Integer
utctMonth (UTCTime day _) = fromIntegral m
  where
    (_, m, _) = toGregorian day
