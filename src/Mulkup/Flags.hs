{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Mulkup.Flags (Flags (..), flagParser) where

import Mulkup.Prelude
import Optics.TH
import Options.Applicative
  ( Parser,
    help,
    long,
    short,
    switch,
  )

data Flags = Flags
  {verbose :: Bool}

makeFieldLabelsNoPrefix ''Flags

flagParser :: Parser Flags
flagParser =
  Flags <$> switch (long "verbose" <> short 'v' <> help "Log verbosely.")
