{-# LANGUAGE RecordWildCards #-}

module Mulkup.Logging where

import Colog (Message, Msg (..), Severity (..))
import Colog.Polysemy.Effect (Log, log)
import Data.Text (pack)
import Mulkup.Prelude
import Polysemy (Member, Sem)

msg :: Severity -> Text -> Message
msg msgSeverity msgText = withFrozenCallStack (Msg {msgStack = callStack, ..})

debugMsg :: Text -> Message
debugMsg = withFrozenCallStack (msg Debug)

infoMsg :: Text -> Message
infoMsg = withFrozenCallStack (msg Info)

warningMsg :: Text -> Message
warningMsg = withFrozenCallStack (msg Warning)

errorMsg :: Text -> Message
errorMsg = withFrozenCallStack (msg Error)

exceptionMsg :: Exception e => e -> Message
exceptionMsg = withFrozenCallStack (msg Error . pack . displayException)

logDebug :: Member (Log Message) r => Text -> Sem r ()
logDebug = withFrozenCallStack (log . debugMsg)

logInfo :: Member (Log Message) r => Text -> Sem r ()
logInfo = withFrozenCallStack (log . infoMsg)

logWarning :: Member (Log Message) r => Text -> Sem r ()
logWarning = withFrozenCallStack (log . warningMsg)

logError :: Member (Log Message) r => Text -> Sem r ()
logError = withFrozenCallStack (log . errorMsg)

logException :: Exception e => Member (Log Message) r => e -> Sem r ()
logException = withFrozenCallStack (log . exceptionMsg)
