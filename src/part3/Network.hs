{-|
  This module contains all of the information for running the network. The
  external API consists of a function that starts the network, given an
  @'IORef'@ containing whether or not the window has closed.
-}
{-# LANGUAGE Arrows #-}
module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFw
import Control.Wire
import Data.IORef

-------------------
-- Local Imports --
import Renderable
import Scene
import Quad

----------
-- Code --

{-|
  A wire to construct the scene from its input elements.
-}
makeScene :: Wire s e IO (Quad, Quad) Scene
makeScene = mkSF_ (uncurry Scene)

{-|
  The wire containing the final scene.
-}
sceneWire :: HasTime t s => Wire s () IO a Scene
sceneWire =
  proc _ -> do
    rec q1 <- quad (Left  ()) (CharKey 'W') (CharKey 'S') (CharKey 'A') (CharKey 'D') -< s
        q2 <- quad (Right ()) (UP)          (DOWN)        (LEFT)        (RIGHT)       -< s
        s  <- makeScene                                                               -< (q1, q2)

    returnA -< s

{-|
  Doing the brunt of the work involved in running the network.
-}
runNetwork' :: (HasTime t s, Monoid e, Renderable b) => IORef Bool -> Session IO s -> Wire s e IO a b -> IO ()
runNetwork' closedRef session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (dt, session') <- stepSession session
      (dw, wire'   ) <- stepWire wire dt $ Right undefined

      case dw of
        Left  _     -> return ()
        Right scene -> do
          clear [ColorBuffer]
          render scene
          swapBuffers

          runNetwork' closedRef session' wire'

{-|
  Running the network.
-}
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef =
  runNetwork' closedRef clockSession_ sceneWire
