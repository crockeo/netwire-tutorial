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
isKeyDown :: Enum k => k -> Wire s () IO a ()
isKeyDown k =
  mkGen_ $ \_ -> do
    s <- getKey k
    return $ case s of
      Release -> Left ()
      Press   -> Right ()

{-|
  Wire for if the key to go left is down specifically.
-}
upKeyDown :: Wire s () IO a ()
upKeyDown = isKeyDown $ CharKey 'W'

{-|
  Wire for if the key to go right is down specifically.
-}
downKeyDown :: Wire s () IO a ()
downKeyDown = isKeyDown $ CharKey 'S'

{-|
  Wire for if the key to go left is down specifically.
-}
leftKeyDown :: Wire s () IO a ()
leftKeyDown = isKeyDown $ CharKey 'A'

{-|
  Wire for if the key to go right is down specifically.
-}
rightKeyDown :: Wire s () IO a ()
rightKeyDown = isKeyDown $ CharKey 'D'

{-|
  The acceleration (and therefore deceleration) of the quad.
-}
acceleration :: Wire s () IO a (V2 Float)
acceleration =
  liftA2 V2 xAcc yAcc
  where xAcc :: Wire s () IO a Float
        xAcc  =  pure  0       . leftKeyDown . rightKeyDown
             <|> pure (-speed) . leftKeyDown
             <|> pure ( speed) . rightKeyDown
             <|> pure  0

        yAcc :: Wire s () IO a Float
        yAcc  =  pure  0       . upKeyDown . downKeyDown
             <|> pure ( speed) . upKeyDown
             <|> pure (-speed) . downKeyDown
             <|> pure  0

{-|
  The current velocity of the quad.
-}
velocity :: HasTime t s => Wire s () IO a (V2 Float)
velocity = integral 0 . acceleration

{-|
  The position of the quad, this time in 2 dimensions.
-}
position :: HasTime t s => Wire s () IO a (V2 Float)
position = integral initPos . velocity

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
        Left  _  -> return ()
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
  runNetwork' closedRef clockSession_ position

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
