{-# Language BangPatterns #-}

module Frame.TUI
where

{-
This module offers a text-based interface for user commands. User scripts can
use functions defined here. Any GUI functionality will be implemented on top
of this module.
-}

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans.State.Strict --(modify, gets, State)
import Data.Char
import Data.List
import Data.Maybe
import System.Cmd
import System.Exit
import System.IO

import Frame.FrameOps
import Frame.Types

import Debug.Trace

mainTUI :: IO ()
mainTUI = mainLoop initialState

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

{-
Returns a dot representation of the frames.
-}
dotString :: [Frame] -> String
dotString s = unlines
  [ "digraph frame {"
  , "\tnode [shape=\"Mrecord\"];"
  , concatMap pPrintFrame s
  , "}"
  ]

{-
Pretty prints a frame for DOT display.
-}
pPrintFrame :: Frame -> String
pPrintFrame f = unlines . concat $ [[pPrintFrameContent f], pPrintFrameLinks f]

{-
Pretty prints frame slots.
-}
pPrintFrameContent :: Frame -> String
pPrintFrameContent f = "\t" ++ show n ++ " [\n\t\tcolor = " ++ c ++ ",\n\t\t"
  ++ "label = \"{" ++ n ++ pPrintSlots (frameSlots f) ++ "}\"];\n"
  {-
  [ ["\t", show n, " ["] -- label and start options
  , ["\t\t", "color = ", c, ","] -- color
  , ["\t\t", "label = \"{", n, "\\"] -- frame name
  , pPrintSlots (frameSlots f)
  , ["}\"];"] -- end
  ]
  -}
  where
    n = frameName f
    c = if frameType f == Generic then "red" else "black"

{-
Pretty prints slots of a frame.
-}
pPrintSlots :: [Slot] -> String
pPrintSlots [] = ""
pPrintSlots (s:ss) = "|\\\n" ++ pPrintSlot s ++
  concatMap (\s -> "|\\\n" ++ pPrintSlot s) ss

{-
Pretty prints a slot.
-}
pPrintSlot :: Slot -> String
pPrintSlot s = "{" ++ slotName s ++ "|" ++ v ++ "|" ++ d ++ "|" ++ n ++ "|" ++ a ++ "}"
  where
    getF f g = maybe "-" g $ f s
    v = getF slotValue show
    d = getF slotDefault show
    n = getF slotIfNeeded (const "N")
    a = getF slotIfAdded (const "A")

{-
Pretty prints frame links.
-}
pPrintFrameLinks :: Frame -> [String]
pPrintFrameLinks f = map (\s -> concat ["\t", n, " -> ", show s, ";"]) $ frameChildren f
  where
    n = show $ frameName f

