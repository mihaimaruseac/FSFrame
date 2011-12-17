module Frame.Types
where

{-
This modules contains all the types and data structures used by the `frame`
application. It also contains useful functions for working with those types
(mainly getting values from them or extending them with useful information).
-}

{-
Commands to manipulate the entire frame system (frames or preferences).
-}
data FSCmd
  = FCREATE String String FrameType -- FCREATE frame_name parent_name type
  | FGET String String -- FGET frame_name slot_name
  | FPUT String String [PutType] -- FPUT frame_name slot_name [ptype]
  | FSETPARAMS ParamSetting Bool -- FSETPARAMS paramtype value
  | FGETPARAMS ParamSetting -- FGETPARAMS paramtype
  deriving (Eq, Show, Read)

{-
Put type. Basically, each FPUT action only sets up a single slot field. Select
this field via the `PutType` option.
-}
data PutType
  = PutV Obj -- fill in value for this slot
  | PutD Obj -- fill in default value for this slot
  | PutN Action -- fill in `if-needed` action for this slot
  | PutA Action -- fill in `if-added` action for this slot
  deriving (Eq, Show, Read)

{-
Parameter settings.
-}
data ParamSetting
  = DefaultsEnabled -- select if default values can be used
  | ActionsEnabled -- select if actions can be triggered
  | DefaultsThenNeeded
  -- select if default values are consulted before the action is triggered
  | SearchTypeIsZ -- selects if searching in Z order or in N order
  deriving (Eq, Show, Read)

{-
The internal state of the frame system consists of all frames and the user
preferences.
-}
type FSState = (World, Pref)

{-
This is the initial state: a world containing only the root frame, defaults
and actions are enabled, default values have higher precedence and we are
searching in Z order.
-}
initialState :: FSState
initialState = ([rootFrame], Pref True True True True)

{-
A world consists of a list of frames. The relationships between them is stored
in each frame.
-}
type World = [Frame]

{-
A frame has a name, a type and a list of slots. It also contains a list of
other frames, represented as children of this node when the entire hierarchy
is represented as a DAG. The exact signification of the `frameChildren` list
depends on the `frameType` attribute (it is the inverse of `FrameRel` type.
Also, the inverse relationship is presented via the `frameParent` attribute
(multiple inheritance is not allowed). Operations to work with a frame are
included in `FrameOps.hs`
-}
data Frame = Frame
  { frameName :: String
  , frameType :: FrameType
  , frameSlots :: [Slot]
  , frameChildren :: [Frame]
  , frameParent :: Frame
  } deriving (Read)

{-
The root frame. Cannot be modified or used.
-}
rootFrame :: Frame
rootFrame = Frame "ROOT" Generic [] [] rootFrame

{-
A frame can be of two kinds: generic or individual.
-}
data FrameType = Generic | Individual deriving (Eq, Read)

{-
The relationship between two frames can be either `Member` or `Subset`,
depending on `FrameType`.
-}
data FrameRel = Member | Subset deriving (Eq, Show, Read)

{-
A slot has a name, an optional value (it can be missing and we need to search
for that value when it is requested), an optional default value, an optional
action to be executed when this slot's value is needed and an optional action
to be executed when this slot (or a similar slot higher in the hierarchy) gets
updated.
-}
data Slot = Slot
  { slotName :: String
  , slotValue :: Maybe Obj
  , slotDefault :: Maybe Obj
  , slotIfNeeded :: Maybe Action
  , slotIfAdded :: Maybe Action
  } deriving (Eq, Show, Read)

{-
An object can be everything in our universe: an integer, a float, a string, an
action or another frame.
-}
data Obj
  = I Integer
  | R Double
  | S String
  | A Action
  | F String -- A Frame but keep only its name
  deriving (Eq, Show, Read)

{-
An action is a precompiled program to be executed when triggered by some
conditions. The precompilation and the execution of these actions is handled
in `Action.hs` module.
-}
type Action = String -- TODO: this should be changed

{-
A preference record contains all user preferences regarding the frame system.
It can be changed when needed. Functions for setting and temporarily changing
the preferences are defined in `Preferences.hs` module.
-}
data Pref = Pref
  { prefDefaultsEnabled :: Bool
  -- if True, use default values when searching for missing values
  , prefActionsEnabled :: Bool
  -- if False, no action will be executed until it is switched to True
  , prefDefaultThenNeeded :: Bool
  -- if True, default values have higher priority than if-needed actions
  , prefSearchTypeIsZ :: Bool
  -- if True, search is done in Z order (see `SearchType` below)
  -- TODO: this can be changed
  } deriving (Eq, Show, Read)

{-
A search can be done:
  * in Z order: value, default, if-needed, up or value, if-needed, default, up
    (subject to defaults and actions being enabled);
  * in N order: value, up, default, up, if-needed, up or the reverse (subject
    to defaults and actions being enabled and `prefDefaultThenNeeded` value).
-}
data SearchType = Z | N deriving (Eq, Show, Read)

{-
Instance of class `Show` for `Frame`.
-}
instance Show Frame where
  show (Frame name typ slots ch _) = show typ ++ ":" ++ name
    ++ show (map frameName ch) ++ show slots

{-
Instance of class `Eq` for `Frame`.
-}
instance Eq Frame where
  f1 == f2 = frameName f1 == frameName f2

{-
Instance of class `Show` for `FrameType`.
-}
instance Show FrameType where
  show Individual = "I"
  show Generic = "G"

{-
Utility function to determine the relationship between two frames.
An individual is a member of a generic.
A generic is a subset of another generic.
All other cases are errors.
-}
getFrameRel :: Frame -> Frame -> FrameRel
getFrameRel f1@(Frame {frameType = ft1}) f2@(Frame {frameType = ft2})
  | ft1 == Individual && ft2 == Generic = Member
  | ft1 == Generic && ft2 == Generic = Subset
  | otherwise = error $ "No relationship between " ++ show f1 ++ " and " ++ show f2

{-
Utility function to convert from a `prefSearchTypeIsZ` to a `SearchType`
value.
-}
getSearchType :: Bool -> SearchType
getSearchType True = Z
getSearchType False = N

{-
Returns the world from a `FSState` state.
-}
fsWorld = fst

{-
Returns the preferences from a `FSState` state.
-}
fsPrefs = snd

