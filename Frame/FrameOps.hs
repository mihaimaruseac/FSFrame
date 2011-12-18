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

import Frame.Action
import Frame.Types

import Debug.Trace
test =
  execState (fcreate "Roman" Individual "Truck") $
  execState (fcreate "Logan" Individual "GermanCar") $
  execState (fcreate "BMW" Individual "GermanCar") $
  execState (fcreate "Toyota" Individual "JapaneseCar") $
  execState (fcreate "GermanCar" Generic "Car") $
  execState (fcreate "JapaneseCar" Generic "Car") $
  execState (fcreate "Truck" Generic "Vehicle") $
  execState (fcreate "Car" Generic "Vehicle") $
  execState (fcreate "Vehicle" Generic "ROOT") initialState
testput =
  execState (fput "Vehicle" "fuelConsumption" (Just (R 4.2)) Nothing Nothing Nothing) $
  execState (fput "Vehicle" "rom_price" Nothing Nothing (Just "-1") Nothing) $ -- TODO: implement proper action
  execState (fput "Roman" "rom_price" (Just (I 52)) Nothing Nothing Nothing) $
  execState (fput "Logan" "rom_price" (Just (I 42)) Nothing Nothing Nothing) $
  execState (fput "Logan" "country" (Just (S "Romania")) Nothing Nothing Nothing) $
  execState (fput "Roman" "country" (Just (S "Romania")) Nothing Nothing Nothing) $
  execState (fput "GermanCar" "country" (Just (S "Germany")) Nothing Nothing Nothing) $
  execState (fput "JapaneseCar" "country" (Just (S "Japan")) Nothing Nothing Nothing) $
  execState (fput "Vehicle" "country" Nothing (Just (S "?")) Nothing Nothing) test

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
  -- 1. check validity of names
  checkValidName sname -- fname should exist, thus not checking
  -- 2. get frame to modify
  w <- gets fsWorld
  let f = getFrameNamed w fname
  -- 3. get initial slot (if there is a slot, otherwise get a Nothing)
  let is = getSlotNamed f sname
  -- 4. build resulting slot
  let s = combineSlots is $ Slot sname value defaultval ifneeded ifadded
  -- 5. update frame with new slot
  let f' = updateFrameSlot f s
  -- 6. update world
  modify . first $ \w -> f' : filter (\f -> frameName f /= fname) w
  -- 7. search and execute `if-added` actions TODO

{-
Evaluates a `FGET` command.
-}
fget :: String -> String -> State FSState Obj
fget fname sname = do
  -- 1. get preferences
  prefs <- gets fsPrefs
  -- 2. start search and get basic value
  o <- if prefSearchTypeIsZ prefs
         then fgetZ fname sname
         else fgetN fname sname
  -- 3. extract value and execute action if returned
  when (isNothing o) $ error $ "Slot " ++ sname ++ " not found."
  case fromJust o of
    A a -> executeAction a
    x -> return x

{-
Retrieves an attirbute using the Z order.
-}
fgetZ = undefined

{-
Retrieves an attirbute using the N order.
-}
fgetN = undefined

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


