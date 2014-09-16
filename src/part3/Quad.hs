{-|
  This module holds all of the information for moving quads around. Something
  to note is that they refer to each other through extracting their respective
  values out of the @'Scene'@ that is passed to them. This third part is
  honestly all an excuse to test (on a small scale) a possible solution to the
  refactoring problem I've faced with Netwire.
-}
{-# LANGUAGE Arrows #-}
module Quad (quad) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (position)
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Linear.V2

-------------------
-- Local Imports --
import Config
import Input
import Scene

----------
-- Code --

{-|
  Directional acceleration. The first key is going positive speed, the second
  key is going negative speed.
-}
dAccel :: (Enum k, Monoid e) => k -> k -> Wire s e IO a Float
dAccel k1 k2  =  pure 0        . isKeyDown k1 . isKeyDown k2
             <|> pure ( speed) . isKeyDown k1
             <|> pure (-speed) . isKeyDown k2
             <|> pure 0

{-|
  The acceleration of the quad.
-}
accel :: (Enum k, Monoid e) => k -> k -> k -> k -> Wire s e IO a (V2 Float)
accel upKey downKey leftKey rightKey =
  liftA2 V2 (dAccel rightKey leftKey)
            (dAccel    upKey downKey)

{-|
  The velocity of the quad.
-}
velocity :: HasTime t s => Wire s e IO (V2 Float) (V2 Float)
velocity = integral 0

{-|
  The position of the quad.
-}
position :: HasTime t s => Wire s e IO (V2 Float) (V2 Float)
position = integral 0

{-|
  Getting the correct quad given the side and the @'Scene'@.
-}
getCorrectQuad :: Either a b -> Wire s e IO Scene Quad
getCorrectQuad (Left  _) = mkSF_ getQuad2
getCorrectQuad (Right _) = mkSF_ getQuad1

{-|
  The color of the quad. Both quads should change to red when they're
  overlapping.
-}
quadColor :: Wire s e IO (Quad, Quad) (Color3 GLfloat)
quadColor =
  mkSF_ $ \(q1, q2) ->
    if overlaps q1 q2
      then Color3 1.0 0.0 0.0
      else Color3 1.0 1.0 1.0
  where overlaps :: Quad -> Quad -> Bool
        overlaps (Quad (V2 x1 y1) (V2 w1 h1) _) (Quad (V2 x2 y2) (V2 w2 h2) _) =
          horizontalCollision && verticalCollision
          where horizontalCollision = x1 < (x2 + w2) && (x1 + w1) > x2
                verticalCollision   = y1 < (y2 + h2) && (y1 + h1) > y2

makeQuad :: Wire s e IO (V2 Float, V2 Float, Color3 GLfloat) Quad
makeQuad = mkSF_ (\(p, s, c) -> Quad p s c)

{-|
  The external type referenced.
-}
quad :: (Enum k, HasTime t s, Monoid e) => Either () () -> k -> k -> k -> k -> Wire s e IO Scene Quad
quad side upKey downKey leftKey rightKey =
  proc scene -> do
    rec a  <- accel upKey downKey leftKey rightKey -< undefined
        v  <- velocity                             -< a
        p  <- position                             -< v
        cq <- getCorrectQuad side                  -< scene
        c  <- quadColor                            -< (q, cq)
        q  <- makeQuad                             -< (p, quadSize, c)

    returnA -< Quad p quadSize c
