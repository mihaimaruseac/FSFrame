module Frame.GUI
where

{-
This modules is used to construct the GUI of the `frame` application. It
contains all functions needed to build the GUI and interact with it but it
exports only the main building function to the outside.
-}

import Frame.TUI

{-
The main GUI building function.
-}
frameGUI :: IO ()
frameGUI = mainTUI --print "ok"

