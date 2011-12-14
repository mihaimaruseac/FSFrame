module Frame
  (module Frame.GUI
  ) where

{-
This modules is used to reexport the `frameGUI` function such that `Main.hs`
can see it and declare it to be the main entry point of the `frame`
application.
-}

import Frame.GUI (frameGUI)

