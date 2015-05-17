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

defaultWindow = ("MOOSE", 640, 480)

run :: WindowSettings -> (Window -> IO resource) -> (resource -> IO ()) -> (resource -> resource) -> IO ()
run (title, w, h) setup draw tick = do
  success <- GLFW.init
  unless success $ putStrLn "Failed to init GLFW."
  GLFW.setErrorCallback (Just onError)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core);
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True);
  window <- GLFW.createWindow (fromInteger w) (fromInteger h) title Nothing Nothing
  case window of
   (Just w) -> do resource <- setup w
                  Just t <- GLFW.getTime
                  gameLoop w draw tick resource t 0
   Nothing -> do putStrLn "Failed to create window."

gameLoop :: Window -> (resource -> IO ()) -> (resource -> resource) -> resource -> Double -> Integer -> IO ()
gameLoop window draw tick resources t frameCount = do
  Just newT <- GLFW.getTime
  let dt = newT - t
  when (frameCount `mod` 30 == 0) (putStrLn $ "FPS: " ++ show (1.0 / dt))
  close <- GLFW.windowShouldClose window
  if close then do
    GLFW.destroyWindow window
    GLFW.terminate
  else do
    GL.clear [GL.ColorBuffer]
    draw resources
    GLFW.swapBuffers window
    GLFW.pollEvents
    let newResources = tick resources
    gameLoop window draw tick newResources newT (frameCount + 1)

onError e message = putStrLn $ "ERROR!" ++ message
  
