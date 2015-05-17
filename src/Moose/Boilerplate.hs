module Moose.Boilerplate ( GLFW.Window
                         , run
                         , WindowSettings
                         , defaultWindow
                         ) where

import Control.Monad (when, unless)
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.BufferObjects (BufferObject)
import Graphics.GLUtil.BufferObjects (makeBuffer, offset0)
import Graphics.GLUtil.ShaderProgram (ShaderProgram, program, simpleShaderProgramBS, enableAttrib, setAttrib)
import Graphics.GLUtil (setUniform)
import Graphics.GLUtil.VertexArrayObjects (VAO, makeVAO, withVAO)
import Foreign.Storable (sizeOf)
import Graphics.Rendering.OpenGL.Raw.ARB.DrawInstanced (glDrawArraysInstanced)
import Graphics.Rendering.OpenGL.Raw (gl_TRIANGLES, gl_LINE_STRIP, gl_TRIANGLE_STRIP)
import Graphics.Rendering.OpenGL.Raw.ARB.InstancedArrays (glVertexAttribDivisor)
import Graphics.Rendering.OpenGL.Raw -- (glVertexAttribPointer)
import Foreign
import Foreign.C.String
import Foreign.C.Types

type WindowSettings = (String, Integer, Integer)
type Draw s = (s -> IO ())
type Tick s = (s -> s)
type OnKey s = s -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> s

defaultWindow = ("MOOSE", 640, 480)

run :: WindowSettings -> (Window -> IO s) -> Draw s -> Tick s -> OnKey s -> IO ()
run (title, w, h) setup draw tick onKey = do
  success <- GLFW.init
  unless success $ putStrLn "Failed to init GLFW."
  GLFW.setErrorCallback (Just onError)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core);
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True);
  window <- GLFW.createWindow (fromInteger w) (fromInteger h) title Nothing Nothing
  case window of
   (Just win) -> do GLFW.makeContextCurrent (Just win)
                    state <- setup win
                    GLFW.setKeyCallback win (Just (keyboardInput state onKey))
                    Just t <- GLFW.getTime
                    gameLoop win draw tick state t 0
   Nothing -> do putStrLn "Failed to create window."

keyboardInput :: s -> OnKey s -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyboardInput s onKey w k n t m = do
  let newState = onKey s w k n t m
  return ()

gameLoop :: Window -> Draw s -> Tick s -> s -> Double -> Integer -> IO ()
gameLoop window draw tick state t frameCount = do
  Just newT <- GLFW.getTime
  let dt = newT - t
  when (frameCount `mod` 30 == 0) (putStrLn $ "FPS: " ++ show (1.0 / dt))
  close <- GLFW.windowShouldClose window
  if close then do
    GLFW.destroyWindow window
    GLFW.terminate
  else do
    GL.clear [GL.ColorBuffer]
    draw state
    GLFW.swapBuffers window
    GLFW.pollEvents
    let newState = tick state
    gameLoop window draw tick newState newT (frameCount + 1)

onError e message = putStrLn $ "ERROR!" ++ message
  
