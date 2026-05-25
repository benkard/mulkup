{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Mulkup.Config (TierConfig (..), TierConfigs (..), StashConfig (..), MulkupConfig (..), readConfig) where

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

data MulkupConfig = MulkupConfig {host :: Text, stashes :: [StashConfig], verbose :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

makeFieldLabelsNoPrefix ''MulkupConfig

--- readConfig ---

-- | Reads a Dhall config, merging the user's record over a defaults
-- record so optional fields can be omitted entirely.
readConfig :: Text -> IO MulkupConfig
readConfig text = Dhall.input auto ("{ verbose = False } // (" <> text <> ")")
