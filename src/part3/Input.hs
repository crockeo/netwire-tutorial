{-|
  This module's sole purpose is to provide APIs to allow people to work with
  GLFW's input API through Netwire.
-}
module Input where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Control.Wire

----------
-- Code --

{-|
  Blocks if a given key is down.
-}
isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a a
isKeyDown k =
  mkGen_ $ \a -> do
    state <- getKey k
    return $ case state of
      Release -> Left  mempty
      Press   -> Right a
