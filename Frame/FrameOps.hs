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
  unless (parent `elem` wnames) $ error $ "Parent `" ++ parent ++ "` does not exist."
  let p = head $ filter (\w -> frameName w == parent) w
  -- 4. update world
  modifyWorld $ addFrameToWorld name p typ

{-
Evaluates a `FPUT` command.
-}
fput :: String -> String -> Maybe Obj -> Maybe Obj -> Maybe Action ->
  Maybe Action -> State FSState ()
fput fname sname value defaultval ifneeded ifadded = do
  when (fname == gROOT) $ error "Root frame cannot be modified."
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
  modifyWorld $ \w -> f' : filter (\f -> frameName f /= fname) w
  -- 7. search and execute `if-added` actions TODO

{-
Evaluates a `FGET` command.
-}
fget :: String -> String -> State FSState Obj
fget fname sname = do
  when (fname == gROOT) $ error "Root frame has no attributes."
  -- 1. get preferences and starting frame
  prefs <- gets fsPrefs
  w <- gets fsWorld
  let f = getFrameNamed w fname
  -- 2. start search and get basic value
  o <- if prefSearchTypeIsZ prefs
         then fgetZ w prefs f sname
         else fgetN w prefs fname sname
  -- 3. extract value and execute action if returned
  when (isNothing o) $ error $ "Slot `" ++ sname ++ "` not found."
  case fromJust o of
    A a -> executeAction prefs a
    x -> return x

{-
Retrieves an attribute using the Z order.
-}
fgetZ :: World -> Pref -> Frame -> String -> State FSState (Maybe Obj)
fgetZ w pref frame sname
  | frame == rootFrame = return Nothing
  | otherwise = maybe (fgetZ w pref (getParentFrame w frame) sname)
                (getValueZFromSlot w pref frame) $ getSlotNamed frame sname

{-
Retrieves a value from a slot in Z order. Calls fgetZ for parent if value is
not found (or it exists but it is disabled).
-}
getValueZFromSlot :: World -> Pref -> Frame -> Slot -> State FSState (Maybe Obj)
getValueZFromSlot w p f s = maybe search retfct $ slotValue s
  where
    retfct = return . Just
    act_result a = executeAction p a >>= retfct
    search_parent = fgetZ w p (getParentFrame w f) (slotName s)
    search = if prefDefaultThenNeeded p then sDNP else sNDP
    sDNP = maybe sNP retfct $ getSlotDefault p s
    sNDP = maybe sDP act_result $ getSlotIfNeeded p s
    sNP = maybe search_parent act_result $ getSlotIfNeeded p s
    sDP = maybe search_parent retfct $ getSlotDefault p s

{-
Retrieves an attribute using the N order.
-}
fgetN :: World -> Pref -> String -> String -> State FSState (Maybe Obj)
fgetN w p f s
  | f == gROOT = return Nothing
  | otherwise = startNSearch w p (getFrameNamed w f) s

{-
Starts a N search. Effectively launches the searches to the root of the tree
until an answer is found.
-}
startNSearch :: World -> Pref -> Frame -> String -> State FSState (Maybe Obj)
startNSearch w p f s = if prefDefaultThenNeeded p then sVDN else sVND
  where
    retfct = return . Just
    act_result a = executeAction p a >>= retfct
    sVDN = maybe stDN retfct $ doNSearch slotValue w f s
    sVND = maybe stND retfct $ doNSearch slotValue w f s
    stDN = if prefDefaultsEnabled p then sDN else stN
    stND = if prefActionsEnabled p then sND else stD
    stN = if prefActionsEnabled p then sN else return Nothing
    stD = if prefDefaultsEnabled p then sD else return Nothing
    sDN = maybe sN retfct $ doNSearch slotDefault w f s
    sND = maybe sD act_result $ doNSearch slotIfNeeded w f s
    sN = maybe (return Nothing) act_result $ doNSearch slotIfNeeded w f s
    sD = return $ doNSearch slotDefault w f s

{-
Search until a value is found or root is reached. Use a selector to look at
values.
-}
doNSearch :: (Slot -> Maybe a) -> World -> Frame -> String -> Maybe a
doNSearch sel w f sn
  | f == rootFrame = Nothing
  | otherwise = maybe parent ((`mplus` parent) . sel) $ getSlotNamed f sn
  where
    parent = doNSearch sel w (getParentFrame w f) sn

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
getFrameNamed [] fname = error $ "No frame named `" ++ fname ++ "`."
getFrameNamed (f:fs) fname
  | frameName f == fname = f
  | otherwise = getFrameNamed fs fname

{-
Returns the parent of a frame.
-}
getParentFrame :: [Frame] -> Frame -> Frame
getParentFrame w f = getFrameNamed w (frameParent f)

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
  | all isAllowed s && isAlpha (head s) = return ()
  | otherwise = error $ "Invalid name `" ++ s ++ "`."
  where
    isAllowed x = isAlphaNum x || x `elem` "_"

{-
Adds a frame to the world and updates the parent.
-}
addFrameToWorld :: String -> Frame -> FrameType -> [Frame] -> [Frame]
addFrameToWorld n p t w = f : p' : w'
  where
    w' = filter (/= p) w
    f = Frame n t [] [] (frameName p)
    p' = let sons = frameChildren p in p { frameChildren = n : sons }

{-
Checks for duplicate frame names.
-}
checkFrameName :: String -> [String] -> State FSState ()
checkFrameName name wnames
  = when (name `elem` wnames) $ error $ "Duplicate frame name `" ++ name ++ "`."

