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
import Debug.Trace

----------
-- Code --

{-|
  The size of the quad.
-}
s :: Float
s = 0.05

{-|
  The global accelerational speed.
-}
speed :: Float
speed = 1.0

{-|
  The minimum speed.
-}
minSpeed :: Float
minSpeed = 0.01

{-|
  The maximum speed.
-}
maxSpeed :: Float
maxSpeed = 4.0

{-|
  The initial position.
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
    s <- getKey k
    return $ case s of
      Release -> Left  mempty
      Press   -> Right a
{-|
  Wire for if the key to go left is down specifically.
-}
leftKeyDown :: Monoid e => Wire s e IO a a
leftKeyDown = isKeyDown $ CharKey 'A'

{-|
  Wire for if the key to go right is down specifically.
-}
rightKeyDown :: Monoid e => Wire s e IO a a
rightKeyDown = isKeyDown $ CharKey 'D'

{-|
  Performing some action with an input.
-}
withInput :: (a -> b) -> Wire s e m a b
withInput fn = mkPure_ $ \a -> Right $ fn a

{-|
  Generating two different directional acceleration / deceleration functions.
-}
dAcceleration :: (Enum k, Monoid e) => k -> k -> Wire s e IO Float Float
dAcceleration k1 k2  =  withInput decel . isKeyDown k1 . isKeyDown k2
                    <|> pure ( speed)   . isKeyDown k1
                    <|> pure (-speed)   . isKeyDown k2
                    <|> withInput decel
  where decel :: Float -> Float
        decel x
          | x <  (-minSpeed)                     = ( speed)
          | x >  ( minSpeed)                     = (-speed)
          | x >= (-minSpeed) && x <= ( minSpeed) = (-x)


{-|
  The acceleration of the quad.
-}
acceleration :: Monoid e => Wire s e IO Float Float
acceleration  =  withInput decel . leftKeyDown . rightKeyDown
             <|> pure (-speed)   . leftKeyDown
             <|> pure ( speed)   . rightKeyDown
             <|> withInput decel
  where decel :: Float -> Float
        decel x
          | x < (-minSpeed)     = ( speed)
          | x > ( minSpeed)     = (-speed)
          | x > (-minSpeed) && x < ( minSpeed) = (-x)
          | otherwise = 0

{-|
  The velocity of the quad.
-}
velocity :: (HasTime t s, Monoid e) => Wire s e IO (Float, Float) (V2 Float)
velocity = integral 0 . withInput (uncurry V2)

{-|
  The position on the quad.
-}
position :: (HasTime t s, Monoid e) => Wire s e IO (V2 Float) (V2 Float)
position = integral 0

{-|
  The final position of the quad.
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
            mapM_ (\(V2 x y)-> vertex $ Vertex2 (realToFrac x :: GLfloat) (realToFrac y :: GLfloat))
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
