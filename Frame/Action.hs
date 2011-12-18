module Frame.Action
where

{-
This module implements needed functionality for actions in the frame system.
-}

import Control.Monad.Trans.State.Strict --(modify, gets, State)

import Frame.Types

{-
Execute a simple action. TODO: define it.
-}
executeAction :: Action -> State FSState Obj
executeAction = undefined

