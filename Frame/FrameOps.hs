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
  -- 0. check conditions
  w <- gets fsWorld
  let wnames = map frameName w
  checkName name wnames
  checkAllParentsKnown parents wnames
  checkParentCount typ $ length parents
  let ps = filter (\w -> (frameName w) `elem` parents) w
  checkNoIndividualParent ps
  -- 1. Build one frame
  let f = Frame name typ [] [] []
  -- TODO: update list of parents
  -- TODO: update parents' list of children
  -- 2. Add frame to world
  modify . first $ (:) f

{-
Checks for duplicate frame names.
-}
checkName :: String -> [String] -> State FSState ()
checkName name wnames = do
  case name `elem` wnames of
    True -> error $ "Duplicate frame name " ++ name
    _ -> return ()

{-
Checks for frame of type `Individual` being parents of others.
-}
checkNoIndividualParent :: [Frame] -> State FSState ()
checkNoIndividualParent [] = return ()
checkNoIndividualParent (f:fs)
  | frameType f == Individual = error $ "Individuals cannot be parents: " ++ fn
  | otherwise = return ()
  where
    fn = frameName f

{-
A `Generic` frame can have any number of parents, including zero. An
`Individual` frame must have at least one parent.
-}
checkParentCount :: FrameType -> Int -> State FSState ()
checkParentCount Individual 0 = error "Individuals must have at least one parent."
checkParentCount _ _ = return ()

{-
All declared parents for a frame must exist.
-}
checkAllParentsKnown :: [String] -> [String] -> State FSState ()
checkAllParentsKnown [] wnames = return ()
checkAllParentsKnown (n:ns) wnames
  | n `elem` wnames = checkAllParentsKnown ns wnames
  | otherwise = error $ "Parent does not exist: " ++ n

