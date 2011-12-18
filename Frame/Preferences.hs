module Frame.Preferences
where

{-
This module implements functions to change and retrieve the preferences
regarding a frame system.
-}

import Control.Monad.Trans.State.Strict

import Frame.Types

{-
Executes a `FSETPARAMS` operation.
-}
fSetParams :: ParamSetting -> Bool -> State FSState ()
fSetParams ps bv = modifyPref $ updatePrefs ps bv

{-
Executes a `FGETPARAMS` operation.
-}
fGetParams :: ParamSetting -> State FSState Bool
fGetParams ps = fmap (readPrefs ps) $ gets fsPrefs

{-
Reads preferences.
-}
readPrefs :: ParamSetting -> Pref -> Bool
readPrefs DefaultsEnabled p = prefDefaultsEnabled p
readPrefs ActionsEnabled p = prefActionsEnabled p
readPrefs DefaultsThenNeeded p = prefDefaultThenNeeded p
readPrefs SearchTypeIsZ p = prefSearchTypeIsZ p

{-
Updates preferences.
-}
updatePrefs :: ParamSetting -> Bool -> Pref -> Pref
updatePrefs DefaultsEnabled b p = p { prefDefaultsEnabled = b }
updatePrefs ActionsEnabled b p = p { prefActionsEnabled = b }
updatePrefs DefaultsThenNeeded b p = p { prefDefaultThenNeeded = b }
updatePrefs SearchTypeIsZ b p = p { prefSearchTypeIsZ = b }

