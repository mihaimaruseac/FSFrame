module Frame.FrameOps
where

{-
This module holds the definitions for each operation working on a frame or the
entire world.
-}

import Control.Arrow --(first)
import Control.Monad
import Control.Monad.Trans.State.Strict --(modify, gets, State)
import Data.Char
import Data.Maybe

import Frame.Types

import Debug.Trace
test = execState (fcreate "zxcv" Individual ["asdf"]) $ execState (fcreate "qwer" Individual ["asdf"]) $ execState (fcreate "asdf" Generic []) initialState
testput = execState (fput "zxcv" "mm" (Just (I 42)) Nothing Nothing Nothing) test

{-
Evaluates a `FCREATE` command.
-}
fcreate :: String -> FrameType -> String -> State FSState ()
fcreate name typ parent = do
  -- 1. check name validity
  checkValidName name
  -- 2. check if frame exists
  w <- gets fsWorld
  let wnames = map frameName w
  checkFrameName name wnames
  -- 3. check if parent exists
  unless (parent `elem` wnames) $ error $ "Parent does not exist: " ++ parent
  let p = head $ filter (\w -> frameName w == parent) w
  -- 4. update world
  modify . first $ addFrameToWorld name p typ

{-
Evaluates a `FPUT` command.
-}
fput :: String -> String -> Maybe Obj -> Maybe Obj -> Maybe Action ->
  Maybe Action -> State FSState ()
fput fname sname value defaultval ifneeded ifadded = do
  -- 0. check conditions
  checkValidName sname -- fname should exist, thus not checking
  w <- gets fsWorld
  let f = getFrameNamed w fname
  -- 1. get initial slot (if there is a slot, otherwise get a Nothing)
  let is = getSlotNamed f sname
  -- 2. build resulting slot
  let s = combineSlots is $ Slot sname value defaultval ifneeded ifadded
  -- 3. update frame with new slot
  let f' = updateFrameSlot f s
  -- 4. update world
  modify . first $ \w -> f' : filter (\f -> frameName f /= fname) w

{-
Updates one frame's slot with the given one. Removes any existing slot with
the same name and cons the given slot to the list of slots.
-}
updateFrameSlot :: Frame -> Slot -> Frame
updateFrameSlot f s = f { frameSlots = ss }
  where
    n = slotName s
    ss = s : filter (\x -> slotName x /= n) (frameSlots f)

{-
Composes two slots with the same name (checked by the caller). The first
argument can be a Nothing in which case copy the second one. Otherwise combine
both values, keeping the non-Nothing ones.
-}
combineSlots :: Maybe Slot -> Slot -> Slot
combineSlots Nothing s = s
combineSlots (Just s) s' = Slot name value defval ifn ifa
  where
    name = slotName s -- same as slotName s'
    value = slotValue s' `mplus` slotValue s'
    defval = slotDefault s' `mplus` slotDefault s
    ifn = slotIfNeeded s' `mplus` slotIfNeeded s
    ifa = slotIfAdded s' `mplus` slotIfAdded s

{-
Gets a frame given its name.
-}
getFrameNamed :: [Frame] -> String -> Frame
getFrameNamed [] fname = error $ "No frame named " ++ fname
getFrameNamed (f:fs) fname
  | frameName f == fname = f
  | otherwise = getFrameNamed fs fname

{-
Gets a slot from a given frame, given by its name. Doesn't return an error
since it is called as an auxiliary function where no errors must be thrown.
-}
getSlotNamed :: Frame -> String -> Maybe Slot
getSlotNamed f sname
  = let this_slot = filter (\s -> slotName s == sname) $ frameSlots f in
    case this_slot of
      [] -> Nothing
      [s] -> Just s
      _ -> error "The impossible happened in getSlotNamed."

{-
Checks if a name is valid.
-}
checkValidName :: String -> State FSState ()
checkValidName s
  | all isAlphaNum s && isAlpha (head s) = return ()
  | otherwise = error $ "Invalid name <" ++ s ++ ">"

{-
Adds a frame to the world and updates the parent.
-}
addFrameToWorld :: String -> Frame -> FrameType -> [Frame] -> [Frame]
addFrameToWorld n p t w = f : p' : w'
  where
    w' = filter (/= p) w
    f = Frame n t [] [] p
    p' = let sons = frameChildren p in p { frameChildren = f : sons }

{-
Checks for duplicate frame names.
-}
checkFrameName :: String -> [String] -> State FSState ()
checkFrameName name wnames
  | name `elem` wnames = error $ "Duplicate frame name " ++ name
  | otherwise = return ()


