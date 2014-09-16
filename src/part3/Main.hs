{-|
  This module starts up the program, and then passes it off to the Netwire
  network.
-}
module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef

-------------------
-- Local Imports --
import Network

----------
-- Code --

{-|
  Starting the program.
-}
main :: IO ()
main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "netwire-tutorial part 3"

  closedRef <- newIORef False
  windowCloseCallback $= do
    writeIORef closedRef True
    return True

  runNetwork closedRef

  closeWindow
