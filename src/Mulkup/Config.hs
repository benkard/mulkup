{-# LANGUAGE UndecidableInstances #-}

module Mulkup.Config (TierConfig (..), MulkupConfig (..), readConfig) where

import Dhall
import Mulkup.Prelude
import Optics.TH

--- TierConfig ---

data TierConfig = TierConfig {keep :: Natural}
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

makeFieldLabelsNoPrefix ''TierConfig

--- TierConfigs ---

data TierConfigs = TierConfigs {hourly :: TierConfig, daily :: TierConfig, weekly :: TierConfig, monthly :: TierConfig}
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

makeFieldLabelsNoPrefix ''TierConfigs

--- StashConfigs ---

data StashConfig = StashConfig {name :: Text, baseDir :: Text, tiers :: TierConfigs, exclusions :: [Text]}
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

makeFieldLabelsNoPrefix ''StashConfig

--- MulkupConfig ---

data MulkupConfig = MulkupConfig {host :: Text, stashes :: [StashConfig]}
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

makeFieldLabelsNoPrefix ''MulkupConfig

--- readConfig ---

readConfig :: Text -> IO MulkupConfig
readConfig = Dhall.input auto
