module Frame.FrameOps
where

{-
This module holds the definitions for each operation working on a frame or the
entire world.
-}

import Control.Arrow --(first)
import Control.Monad.Trans.State.Strict --(modify, gets, State)

import Frame.Types

{-
Evaluates a `FCREATE` command.
-}
fcreate :: String -> FrameType -> [String] -> State FSState ()
fcreate name typ parents = do
  -- TODO: check for collisions
  -- 1. Build one frame
  let f = Frame name typ [] [] []
  -- TODO: update list of parents
  -- TODO: update parents' list of children
  -- 2. Add frame to world
  modify . first $ (:) f

