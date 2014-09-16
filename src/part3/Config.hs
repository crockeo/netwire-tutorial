{-|
  This module provides global flags that define important parts of the project,
  such as speed.
-}
module Config where

--------------------
-- Global Imports --
import Linear.V2

----------
-- Code --

{-|
  The speed of both of the given quads.
-}
speed :: Float
speed = 2.0

{-|
  The size of a quad.
-}
quadSize :: V2 Float
quadSize = V2 0.1 0.1
