module Mulkup.Prelude
  ( module Relude,
    module Relude.Extra.Group,
    module Relude.Extra.Map,
  )
where

import Relude hiding (Reader, ask, asks, local, runReader)
import Relude.Extra.Group
import Relude.Extra.Map
