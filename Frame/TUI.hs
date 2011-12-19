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

