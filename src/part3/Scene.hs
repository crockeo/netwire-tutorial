{-|
  The datatypes that compose the scene are housed within this module.
-}
module Scene where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Linear.V2

-------------------
-- Local Imports --
import Renderable

----------
-- Code --

{-|
  The quad data type, holds position and size.
-}
data Quad = Quad { getPos   :: V2 Float
                 , getSize  :: V2 Float
                 , getColor :: Color3 GLfloat
                 }

instance Renderable Quad where
  render (Quad p s c) = do
    color c
    renderPrimitive Quads $
      mapM_ linearVertex $
        generateVertecies p s

{-|
  The scene contains two quads.
-}
data Scene = Scene { getQuad1 :: Quad
                   , getQuad2 :: Quad
                   }

instance Renderable Scene where
  render (Scene q1 q2) = do
    render q1
    render q2
