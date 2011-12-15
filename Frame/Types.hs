module Frame.Types
where

{-
This modules contains all the types and data structures used by the `frame`
application. It also contains useful functions for working with those types
(mainly getting values from them or extending them with useful information).
-}

{-
A frame has a name, a type and a list of slots. It also contains a list of
other frames, represented as children of this node when the entire hierarchy
is represented as a DAG. The exact signification of the `frameChildren` list
depends on the `frameType` attribute (it is the inverse of `FrameRel` type.
Also, the inverse relationship is presented via the `frameParents` list
(multiple inheritance is allowed).
-}
data Frame = Frame
  { frameName :: String
  , frameType :: FrameType
  , frameSlots :: [Slot]
  , frameChildren :: [Frame]
  , frameParents :: [Frame]
  } deriving (Eq, Show, Read)

{-
A frame can be of two kinds: generic or individual.
-}
data FrameType = Generic | Individual deriving (Eq, Show, Read)

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
  | F Frame
  deriving (Eq, Show, Read)

{-
An action is a precompiled program to be executed when triggered by some
conditions. The precompilation and the execution of these actions is handled
in a separate module.
-}
type Action = String -- TODO: this should be changed

{-
A preference record contains all user preferences regarding the frame system.
It can be changed when needed. Functions for setting and temporarily changing
the preferences are defined in a separate module.
-}
data Pref = Pref
  { prefDefaultsEnabled :: Bool
  -- if True, use default values when searching for missing values
  , prefActionsEnabled :: Bool
  -- if False, no action will be executed until it is switched to True
  , prefDefaultThenNeeded :: Bool
  -- if True, default values have higher priority than if-needed actions
  , prefSearchType :: SearchType
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
Utility function to determine the relationship between two frames.
An individual is a member of a generic.
A generic is a subset of another generic.
All other cases are errors.
-}
getFrameRel :: Frame -> Frame -> FrameRel
getFrameRel f1@(Frame {frameType = ft1}) f2@(Frame {frameType = ft2})
  | ft1 == Individual && ft2 == Generic = Member
  | ft1 == Generic && ft2 == Generic = Subset
  | otherwise = error $ "No relationship between " ++ (show f1) ++ " and " ++ (show f2)

