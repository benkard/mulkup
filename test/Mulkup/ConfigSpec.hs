{-# LANGUAGE OverloadedStrings #-}

module Mulkup.ConfigSpec (cases) where

import Mulkup.Config
import Mulkup.Prelude
import Test.Tasty
import Test.Tasty.HUnit

cases :: TestTree
cases =
  testGroup
    "ConfigSpec"
    [unit_simpleConfig]

unit_simpleConfig :: TestTree
unit_simpleConfig = testCase "unit_simpleConfig" $ do
  void $ readConfig exampleConfigText
  where
    exampleConfigText =
      "\
      \{ host = \"atmon\" \
      \ \
      \, stashes = \
      \    [ { name = \"mulk\" \
      \       \
      \      , baseDir = \"/Users/mulk\" \
      \       \
      \      , tiers = \
      \          { hourly  = { keep = 48 } \
      \          , daily   = { keep =  4 } \
      \          , weekly  = { keep =  4 } \
      \          , monthly = { keep = 12 } \
      \          } \
      \       \
      \      , exclusions = \
      \          [ \
      \          , \"**/.stack-work\" \
      \          , \"**/dist-newstyle\" \
      \         \
      \          , \"~/.boot/cache\" \
      \          , \"~/.cabal/bin\" \
      \          , \"~/.cabal/packages\" \
      \          , \"~/.cache\" \
      \          ] \
      \      } \
      \    ] \
      \}"
