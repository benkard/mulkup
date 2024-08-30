{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Mulkup.Main where

import Colog (Message, richMessageAction, simpleMessageAction)
import Colog.Polysemy.Effect (Log, runLogAction)
import qualified Data.Set as Set
import Mulkup.Bupstash
import Mulkup.Config
import Mulkup.Flags
import Mulkup.Logging
import Mulkup.Prelude
import Mulkup.Ranking
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

    let (keepIds, rmIds) = rankBupItems (stash ^. #tiers) currentItems

    logInfo (show labels <> " Keeping: " <> show (Set.toList keepIds))
    logInfo (show labels <> " Removing: " <> show (Set.toList rmIds))
    bupRemove (Set.toList rmIds)

    logInfo (show labels <> " Creating backup.")
    bupPut
      (stash ^. #baseDir)
      (stash ^. #exclusions)
      labels
