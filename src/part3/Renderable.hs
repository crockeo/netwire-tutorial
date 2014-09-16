{-|
  Implementing a more dynamic system for something that can be rendered.
-}

{-# LANGUAGE ExistentialQuantification #-}
module Renderable where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (Render)
import Linear.V2

----------
-- Code --

{-|
  The typeclass to define that something is indeed renderable.
-}
class Renderable a where
  render :: a -> IO ()

{-|
  Performing a @'vertex'@ call on a @'V2'@.
-}
linearVertex :: V2 Float -> IO ()
linearVertex (V2 x y) =
  vertex $ Vertex2 (realToFrac x :: GLfloat)
                   (realToFrac y :: GLfloat)

{-|
  Generating a set of rectangular vertecies from position and size vectors.
-}
generateVertecies :: V2 Float -> V2 Float -> [V2 Float]
generateVertecies (V2 x y) (V2 w h) =
  [ V2 (x    ) (y    )
  , V2 (x + w) (y    )
  , V2 (x + w) (y + h)
  , V2 (x    ) (y + h)
  ]
