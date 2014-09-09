{-# LANGUAGE Arrows #-}
module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Data.IORef
import Linear.V2

----------
-- Code --

{-|
  The size of the quad. Technically half of the size of the quad, as its
  rendering is performed like so:

    (x - s, y - s)
    (x + s, y - s)
    (x + s, y + s)
    (x - s, y + s)
-}
s :: Float
s = 0.05

{-|
  The global accelerational (and deceleration) speed.
-}
speed :: Float
speed = 4.0

{-|
  The minimum speed the quad can take before becoming completely stationary
  again. This is used because otherwise, due to floating point inaccuracies,
  you would have a quad shifting back and forth.
-}
minSpeed :: Float
minSpeed = 0.01

{-|
  The initial position the quad takes. The bounds are:

    x min: -1 - s, x max: 1 + s
    y min: -1 - s, y max: 1 + s

  Where is is the global variable s stored above.
-}
initPos :: V2 Float
initPos = pure 0

{-|
  Checking if a given key is held down. The wire blocks when the key is not
  held down, and does not block when the key is held down.
-}
isKeyDown :: (Monoid e, Enum k) => k -> Wire s e IO a a
isKeyDown k =
  mkGen_ $ \a -> do
    state <- getKey k
    return $ case state of
      Release -> Left  mempty
      Press   -> Right a

{-|
  Performing some action with an input. Essentially a short hand version of
  mkPure_ where you don't need to specify that something will not block.
-}
withInput :: (a -> b) -> Wire s e m a b
withInput fn = mkPure_ $ \a -> Right $ fn a

{-|
  Applying deceleration to a @'Float'@.
-}
decel :: Float -> Float
decel x
  | x <  (-minSpeed)                     = ( speed)
  | x >  ( minSpeed)                     = (-speed)
  | otherwise                            = x

{-|
  Generating two different directional acceleration / deceleration functions.
-}
dAcceleration :: (Enum k, Monoid e) => k -> k -> Wire s e IO Float Float
dAcceleration k1 k2  =  withInput decel . isKeyDown k1 . isKeyDown k2
                    <|> pure ( speed)   . isKeyDown k1
                    <|> pure (-speed)   . isKeyDown k2
                    <|> withInput decel

{-|
  The velocity of the quad. Checks for the minimum velocity on the acceleration
-}
velocity :: (HasTime t s, Monoid e) => Wire s e IO (Float, Float) (V2 Float)
velocity =
  withInput stop . integral 0 . withInput (uncurry V2)
  where stop :: V2 Float -> V2 Float
        stop = fmap stopF
          where stopF :: Float -> Float
                stopF x =
                  if x > (-minSpeed) && x < minSpeed
                    then 0
                    else x

{-|
  The current position of the quad. It returns the integral of the input of a
  @'V2'@ @'Float'@. The input should be the current velocity of the block.
-}
position :: (HasTime t s, Monoid e) => Wire s e IO (V2 Float) (V2 Float)
position = integral initPos

{-|
  The final position of the quad. It uses Arrow notation, specifically the rec
  keyword, to recursively define the application of the network. To get the
  delta x, you need to know the current x velocity, and to get the current x
  velocity you need to know the delta x. Conveniently, you can perform this
  sort of recursive definition very easily using Arrow notation, as displayed
  below.
-}
fPos :: HasTime t s => Wire s () IO a (V2 Float)
fPos = proc _ -> do
  rec x            <- dAcceleration (CharKey 'D') (CharKey 'A') -< vx
      y            <- dAcceleration (CharKey 'W') (CharKey 'S') -< vy
      v@(V2 vx vy) <- velocity                                  -< (x, y)
      p            <- position                                  -< v

  returnA -< p

{-|
  Actually running the network, and performing OpenGL calls on the result.
-}
runNetwork' :: IORef Bool -> Session IO s -> Wire s e IO a (V2 Float) -> IO ()
runNetwork' closedRef session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st , session') <- stepSession session
      (dw', wire'   ) <- stepWire wire st $ Right undefined
      case dw' of
        Left  _      -> return ()
        Right (V2 x y) -> do
          clear [ColorBuffer]

          renderPrimitive Quads $
            mapM_ (\(V2 rx ry) -> vertex $ Vertex2 (realToFrac rx :: GLfloat)
                                                   (realToFrac ry :: GLfloat))
                  [ V2 (x - s) (y - s)
                  , V2 (x + s) (y - s)
                  , V2 (x + s) (y + s)
                  , V2 (x - s) (y + s)
                  ]

          swapBuffers

          runNetwork' closedRef session' wire'

{-|
  Simply a wrapper function around @'runNetwork''@. Modeled after how I design
  my API for network modules that I create.
-}
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef =
  runNetwork' closedRef clockSession_ fPos

{-|
  The entry point to the program. It handles creating the GLFW window and the
  OpenGL context. It then passes the main thread over to the @'runNetwork'@
  function where the program begins to execute the Netwire network.
-}
main :: IO ()
main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "netwire-tutorial"

  closedRef <- newIORef False
  windowCloseCallback $= do
    writeIORef closedRef True
    return True

  runNetwork closedRef

  closeWindow
