module Moose.Boilerplate ( GLFW.Window
                         , run
                         , RenderPass(..)
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

data RenderPass = RenderPass VAO ShaderProgram (RenderPass -> IO ()) BufferObject

type WindowSettings = (String, Integer, Integer)

defaultWindow = ("MOOSE", 640, 480)

run :: WindowSettings -> (Window -> IO [RenderPass]) -> IO ()
run (title, w, h) setup = do
  success <- GLFW.init
  unless success $ putStrLn "Failed to init GLFW."
  GLFW.setErrorCallback (Just onError)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core);
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True);
  window <- GLFW.createWindow (fromInteger w) (fromInteger h) title Nothing Nothing
  case window of
   (Just w) -> do r <- setup w
                  Just t <- GLFW.getTime
                  gameLoop w r t 0
   Nothing -> do putStrLn "Failed to create window."

gameLoop :: Window -> [RenderPass] -> Double -> Integer -> IO ()
gameLoop window resources t frameCount = do
  Just newT <- GLFW.getTime
  let dt = newT - t
  when (frameCount `mod` 30 == 0) (putStrLn $ "FPS: " ++ show (1.0 / dt))
  close <- GLFW.windowShouldClose window
  if close then do
    GLFW.destroyWindow window
    GLFW.terminate
  else do
    GL.clear [GL.ColorBuffer]
    mapM_ draw resources
    GLFW.swapBuffers window
    GLFW.pollEvents
    gameLoop window resources newT (frameCount + 1)

onError e message = putStrLn $ "ERROR!" ++ message

draw :: RenderPass -> IO ()
draw r@(RenderPass vao prog renderFn offsets_vbo) =
  withVAO vao (renderFn r)
  
