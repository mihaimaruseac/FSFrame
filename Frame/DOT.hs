module Frame.DOT (dotString)
where

{-
This module contains the code needed to return a graphical representation for
all frames in this system. It exports a single function, `dotString` which
will transform the system to a single DOT string.
-}

import Frame.Types

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
Private, auxiliary functions.
-}

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

