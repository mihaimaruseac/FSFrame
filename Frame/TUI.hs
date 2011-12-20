{-# Language BangPatterns #-}

module Frame.TUI (mainTUI)
where

{-
This module offers a text-based interface for user commands. User scripts can
use functions defined here. Any GUI functionality will be implemented on top
of this module.
-}

import qualified Control.Exception as E (handle, ErrorCall, IOException)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Maybe (isJust, fromJust)
import System.Cmd (system)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

import Frame.DOT
import Frame.FrameOps
import Frame.Types

{-
Main loop of the application.
-}
mainTUI :: IO ()
mainTUI = mainLoop initialState

{-
Private functions. Used to implement the REPL loop of the TUI.
-}

{-
Main REPL loop.
-}
mainLoop :: FSState -> IO ()
mainLoop !s = do
  putStr "> "
  hFlush stdout
  cmd <- getLine
  E.handle (hnd s) (fcmd cmd)
  where
    hnd :: FSState -> E.ErrorCall -> IO ()
    hnd s e = print e >> mainLoop s
    fcmd c = let userCmd = read c in case userCmd of
      QUIT -> exitSuccess
      RUN fname -> batchRun fname s >>= mainLoop
      DUMP -> print s >> mainLoop s
      GRAPH fname -> graph fname s >> mainLoop s
      _ -> do
        let (s', v) = executeCmd Nothing s userCmd
        when (isJust v) $ print $ fromJust v
        mainLoop s'

{-
Batch runs a script to construct the initial frame set.
-}
batchRun :: String -> FSState -> IO FSState
batchRun filename s = do
  c <- E.handle hnd (readFile filename)
  let cmds = map read . filter nonEmpty $ lines c
  return $ foldl' executeCmd' s cmds
  where
    hnd :: E.IOException -> a
    hnd _ = error $ "File not found `" ++ filename ++ "'."
    nonEmpty s = not $ all isSpace s
    executeCmd' s c = fst $ executeCmd Nothing s c

{-
Gets a DOT representation of the frames.
-}
graph :: String -> FSState -> IO ()
graph fname s = do
  let fnamedot = fname ++ ".dot"
  writeFile fnamedot $ dotString $ fsWorld s
  system $ "dot -Tpng " ++ fnamedot ++  " > " ++ fname
  system $ "eog " ++ fname
  return ()

