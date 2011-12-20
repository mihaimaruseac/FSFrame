module Frame
  (module Frame.TUI
  ) where

{-
This modules is used to reexport the `mainTUI` function such that `Main.hs`
can see it and declare it to be the main entry point of the `frame`
application.
-}

import Frame.TUI (mainTUI)

