{-# Language BangPatterns #-}

module Frame.TUI
where

{-
This module offers a text-based interface for user commands. User scripts can
use functions defined here. Any GUI functionality will be implemented on top
of this module.
-}

import Control.Monad
import Control.Monad.Trans.State.Strict --(modify, gets, State)
import Data.Char
import Data.List
import Data.Maybe
import System.Exit

--import Frame.Action
import Frame.FrameOps
--import Frame.Preferences
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

mainTUI :: IO ()
mainTUI = mainLoop initialState

mainLoop :: FSState -> IO ()
mainLoop !s = do
  putStr "> "
  userCmd <- readLn
  case userCmd of
    QUIT -> exitSuccess
    RUN fname -> batchRun fname s >>= mainLoop
    DUMP -> print s >> mainLoop s
    _ -> do
      let (s', v) = executeCmd s userCmd
      when (isJust v) $ print $ fromJust v
      mainLoop s'

batchRun :: String -> FSState -> IO FSState
batchRun filename s = do
  c <- readFile filename
  let cmds = map read . filter nonEmpty $ lines c
  return $ foldl' executeCmd' s cmds
  where
    nonEmpty s = not $ all isSpace s
    executeCmd' s c = fst $ executeCmd s c

