module Frame.FrameOps (executeCmd)
where

{-
This module holds the definitions for each operation working on a frame or the
entire world. It exports only `executeCmd`, all other functions being
implemented by calling this one.
-}

import Control.Arrow (second)
import Control.Monad (unless, when, mplus)
import Control.Monad.Trans.State.Strict (State, get, put, gets, execState, runState)
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Tuple (swap)

--import Frame.Action
import Frame.Preferences
import Frame.Types

{-
Executes an user command. Basically, call helper functions to do the actual
work for us.
-}
executeCmd :: ActionInput -> FSState -> UserCmd -> (FSState, Maybe Obj)
executeCmd _ _ QUIT = error "QUIT is reserved for userspace."
executeCmd _ _ (RUN _) = error "RUN is reserved for userspace."
executeCmd _ _ DUMP = error "DUMP is reserved for userspace."
executeCmd _ _ (GRAPH _) = error "GRAPH is reserved for userspace."
executeCmd ai s (EXEC fscmd) = (execState (execUserCmd ai fscmd) s, Nothing)
executeCmd ai s (EVAL expr) = second Just $ swap $ runState (evaluateExpr ai expr) s

{-
Private functions.
-}

{-
Evaluates an expression. Can have state effects.
-}
evaluateExpr :: ActionInput -> Expr -> State FSState Obj
evaluateExpr _ (DOT fname sname) = fget fname sname
evaluateExpr _ (PREF pname) = fgetparams pname
evaluateExpr _ (OBJ o) = return o
evaluateExpr ai (NOT expr) = do
  v <- evaluateExpr ai expr
  return . B . not . unB $ v
evaluateExpr ai (AND e1 e2) = do
  v1 <- evaluateExpr ai e1
  v2 <- evaluateExpr ai e2
  return . B . all unB $ [v1, v2]
evaluateExpr ai (OR e1 e2) = do
  v1 <- evaluateExpr ai e1
  v2 <- evaluateExpr ai e2
  return . B . any unB $ [v1, v2]
evaluateExpr ai (CONCAT e1 e2) = do
  v1 <- evaluateExpr ai e1
  v2 <- evaluateExpr ai e2
  return . S . foldl1 (++) . map unS $ [v1, v2]
evaluateExpr ai (ADD e1 e2) = do
  v1 <- evaluateExpr ai e1
  v2 <- evaluateExpr ai e2
  return . R $ unR v1 + unR v2
evaluateExpr ai (SUB e1 e2) = do
  v1 <- evaluateExpr ai e1
  v2 <- evaluateExpr ai e2
  return . R $ unR v1 - unR v2
evaluateExpr ai (MUL e1 e2) = do
  v1 <- evaluateExpr ai e1
  v2 <- evaluateExpr ai e2
  return . R $ unR v1 * unR v2
evaluateExpr ai (DIV e1 e2) = do
  v1 <- evaluateExpr ai e1
  v2 <- evaluateExpr ai e2
  return . R $ unR v1 / unR v2
evaluateExpr ai FNAME =
  case ai of
    Just (fname, _, _) -> return . S $ fname
    _ -> error "Cannot get FNAME. Check action."
evaluateExpr ai SNAME =
  case ai of
    Just (_, sname, _) -> return . S $ sname
    _ -> error "Cannot get SNAME. Check action."
evaluateExpr ai SVAL =
  case ai of
    Just (_, _, Just o) -> return o
    _ -> error "Cannot get SVAL. Check action."

{-
Executes a user command having side effects (called via EXEC).
-}
execUserCmd :: ActionInput -> FSCmd -> State FSState ()
execUserCmd _ (FCREATE fname pname typ) = fcreate fname typ pname
execUserCmd ai (FPUT fname sname ptype) = doPut ai fname sname ptype
execUserCmd _ (FSETPARAMS param value) = fsetparams param value

{-
Helper function to put a slot.
-}
doPut :: ActionInput -> String -> String -> PutType -> State FSState ()
doPut _ f s (PutV o) = fput f s (Just o) Nothing Nothing Nothing
doPut _ f s (PutD o) = fput f s Nothing (Just o) Nothing Nothing
doPut _ f s (PutN o) = fput f s Nothing Nothing (Just o) Nothing
doPut _ f s (PutA o) = fput f s Nothing Nothing Nothing (Just o)
doPut ai f s (PutVE e)
  = evaluateExpr ai e >>= \o -> fput f s (Just o) Nothing Nothing Nothing
doPut ai f s (PutDE e)
  = evaluateExpr ai e >>= \o -> fput f s Nothing (Just o) Nothing Nothing

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
  -- 7. search and execute `if-added` actions only if a value got added
  p <- gets fsPrefs
  when (isJust value && prefActionsEnabled p) (searchExecute fname sname value)

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
    A a -> executeAction fname sname Nothing prefs a
    x -> return x

{-
Searches and executes an if-added action.
-}
searchExecute :: String -> String -> Maybe Obj -> State FSState ()
searchExecute fname sname obj = do
  a <- searchAction fname sname
  p <- gets fsPrefs
  executeActionMaybe fname sname obj p a
  return () -- ignore any result in if-added actions

{-
Searches an if-added action.
-}
searchAction :: String -> String -> State FSState Action
searchAction fname sname
  | fname == gROOT = return noAction
  | otherwise = do
    w <- gets fsWorld
    let f = getFrameNamed w fname
    let s = getSlotNamed f sname
    maybe (parent f) (getSlot f) s
  where
    getSlot f s = maybe (parent f) return $ slotIfAdded s
    parent f = searchAction (frameParent f) sname

{-
Execute a simple action.
-}
executeAction :: String -> String -> Maybe Obj -> Pref -> Action -> State FSState Obj
executeAction fname sname obj p a = do
  unless (prefActionsEnabled p) $ error "Action is required but disabled."
  o <- executeActionMaybe fname sname obj p a
  when (isNothing o) $ error "Action didn't return a value."
  return $ fromJust o

{-
Executes a simple action but doesn't fail if no result is produced.
-}
executeActionMaybe :: String -> String -> Maybe Obj -> Pref -> Action -> State FSState (Maybe Obj)
executeActionMaybe fname sname obj p a = do
  o <- evalAct fname sname obj a
  if isNothing o then return Nothing
    else case fromJust o of
      A a -> gets fsPrefs >>= \p -> executeActionMaybe fname sname obj p a
      x -> return $ Just x

{-
Helper function for action running.
-}
evalAct :: String -> String -> Maybe Obj -> [UserCmd] -> State FSState (Maybe Obj)
evalAct _ _ _ [] = return Nothing
evalAct fname sname obj (cmd:cmds) = do
  s <- get
  let (s', o) = executeCmd (Just (fname, sname, obj)) s cmd
  put s'
  r <- evalAct fname sname obj cmds
  return $ r `mplus` o

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
    act_result a = executeAction (frameName f) (slotName s) Nothing p a >>= retfct
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
    act_result a = executeAction (frameName f) s Nothing p a >>= retfct
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

