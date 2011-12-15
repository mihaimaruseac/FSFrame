module Frame.FrameOps
where

{-
This module holds the definitions for each operation working on a frame or the
entire world.
-}

import Control.Arrow --(first)
import Control.Monad.Trans.State.Strict --(modify, gets, State)
import Data.Char

import Frame.Types

import Debug.Trace
test = execState (fcreate "zxcv" Individual ["asdf"]) $ execState (fcreate "qwer" Individual ["asdf"]) $ execState (fcreate "asdf" Generic []) initialState

{-
Evaluates a `FCREATE` command.
-}
fcreate :: String -> FrameType -> [String] -> State FSState ()
fcreate name typ parents = do
  -- 0. check conditions
  checkValidName name
  w <- gets fsWorld
  let wnames = map frameName w
  checkFrameName name wnames
  checkAllParentsKnown parents wnames
  checkParentCount typ $ length parents
  let ps = filter (\w -> frameName w `elem` parents) w
  checkNoIndividualParent ps
  -- 1. Build one frame
  let f = Frame name typ [] [] ps
  -- 2. Update parents' list of children
  modify . first $ map (updateParent f ps)
  -- 3. Add frame to world
  modify . first $ (:) f

{-
Checks if a name is valid.
-}
checkValidName :: String -> State FSState ()
checkValidName s
  | and (map isAlphaNum s) && isAlpha (head s) = return ()
  | otherwise = error $ "Invalid name <" ++ s ++ ">"

{-
Update the parents of a frame.
-}
updateParent :: Frame -> [Frame] -> Frame -> Frame
updateParent s ps p
  | p `elem` ps = let ss = frameChildren p in p { frameChildren = s : ss }
  | otherwise = p

{-
Checks for duplicate frame names.
-}
checkFrameName :: String -> [String] -> State FSState ()
checkFrameName name wnames
  | name `elem` wnames = error $ "Duplicate frame name " ++ name
  | otherwise = return ()

{-
Checks for frame of type `Individual` being parents of others.
-}
checkNoIndividualParent :: [Frame] -> State FSState ()
checkNoIndividualParent [] = return ()
checkNoIndividualParent (f:fs)
  | frameType f == Individual = error $ "Individuals cannot be parents: " ++ fn
  | otherwise = checkNoIndividualParent fs
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

