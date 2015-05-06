module Main where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Control.Monad (when)

main :: IO ()
main = do
  success <- GLFW.init
  if success then putStrLn "Init OK." else putStrLn "Failed to init GLFW."
  window <- GLFW.createWindow 300 300 "MOOSE!" Nothing Nothing
  case window of
   (Just w) -> do setup w
                  gameLoop w
   Nothing -> putStrLn "Failed to create window."

setup :: Window -> IO ()
setup window = do
  GLFW.makeContextCurrent (Just window)
  GLFW.setKeyCallback window (Just keyCallback)
  GL.clearColor $= Color4 0.9 0.2 0.3 1.0

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ =
  when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
    GLFW.setWindowShouldClose window True

gameLoop :: Window -> IO ()
gameLoop window = do
  close <- GLFW.windowShouldClose window
  if close then do
    GLFW.destroyWindow window
    GLFW.terminate
  else do
    GL.clear [GL.ColorBuffer]
    draw
    GLFW.swapBuffers window
    GLFW.pollEvents
    gameLoop window

draw :: IO ()
draw = do
  GL.renderPrimitive GL.Lines $ do
    vertex v1
    vertex v2
  GL.renderPrimitive GL.Lines $ do
    vertex v3
    vertex v4

v1 :: Vertex3 GLfloat
v1 = (GL.Vertex3 (-0.5) (-0.5) 0.0)

v2 :: Vertex3 GLfloat
v2 = (GL.Vertex3 0.5 0.5 0.0)

v3 :: Vertex3 GLfloat
v3 = (GL.Vertex3 (-0.5) 0.5 0.0)

v4 :: Vertex3 GLfloat
v4 = (GL.Vertex3 0.5 (-0.5) 0.0)

