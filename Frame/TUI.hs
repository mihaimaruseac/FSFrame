module Frame.TUI
where

{-
This module offers a text-based interface for user commands. User scripts can
use functions defined here. Any GUI functionality will be implemented on top
of this module.
-}

import Frame.Action
import Frame.FrameOps
import Frame.Preferences
import Frame.Types

import Control.Monad.Trans.State.Strict --(modify, gets, State)

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
  execState (fcreate "Vehicle" Generic gROOT) initialState
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
testget =
  evalState (fget "Car" "country") $
  execState (fput "Car" "country" Nothing Nothing (Just "-2") Nothing) testput

{-
Executes a user command and returns a value. To be called only for FGET
commands. If called for an FGETPARAMS command, the returned value of
`fgetparams` is boxed into a `Obj` type of type `B`.
-}
evalUserCmd :: FSCmd -> State FSState Obj
evalUserCmd (FGET fname sname) = fget fname sname
evalUserCmd cmd = error $ "Command `" ++ show cmd ++ "` cannot return a value."

{-
Executes a user command. To be called only for commands not requiring a value
to be returned. Otherwise, the returned value is ignored.
-}
execUserCmd :: FSCmd -> State FSState ()
execUserCmd (FCREATE fname pname typ) = fcreate fname typ pname
execUserCmd (FGET fname sname) = fget fname sname >> return ()
execUserCmd (FPUT fname sname ptype) = doPut fname sname ptype
execUserCmd (FSETPARAMS param value) = fsetparams param value
execUserCmd (FGETPARAMS param) = fgetparams param >> return ()

{-
Helper function to put a slot.
-}
doPut :: String -> String -> PutType -> State FSState ()
doPut f s (PutV o) = fput f s (Just o) Nothing Nothing Nothing
doPut f s (PutD o) = fput f s Nothing (Just o) Nothing Nothing
doPut f s (PutN o) = fput f s Nothing Nothing (Just o) Nothing
doPut f s (PutA o) = fput f s Nothing Nothing Nothing (Just o)

