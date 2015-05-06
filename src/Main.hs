{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.BufferObjects (BufferObject)
import Graphics.GLUtil.BufferObjects (makeBuffer, offset0)
import Graphics.GLUtil.ShaderProgram (ShaderProgram, program, simpleShaderProgramBS)
import Graphics.GLUtil.VertexArrayObjects (VAO, makeVAO, withVAO)
import Foreign.Storable (sizeOf)

onError e message = do
  putStrLn "ERROR!"
  putStrLn message

main :: IO ()
main = do
  success <- GLFW.init
  unless success $ putStrLn "Failed to init GLFW."
  GLFW.setErrorCallback (Just onError)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core);
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True);
  window <- GLFW.createWindow 300 300 "M O O S E" Nothing Nothing
  case window of
   (Just w) -> do r <- setup w
                  gameLoop w r
   Nothing -> do putStrLn "Failed to create window."
                 
data Resources = Resources GL.Program BufferObject VAO

setup :: Window -> IO Resources
setup window = do
  GLFW.makeContextCurrent (Just window)
  GLFW.setKeyCallback window (Just keyCallback)
  GL.clearColor $= Color4 0.9 0.2 0.3 1.0
  prog <- simpleShaderProgramBS vert frag
  vbo <- makeBuffer ArrayBuffer vertices
  vao <- makeVAO $ do
    GL.currentProgram $= Just (program prog)
    GL.bindBuffer ArrayBuffer $= Just vbo
    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 3
        vad = VertexArrayDescriptor 3 Float stride offset0
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (ToFloat, vad)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    --GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
  return (Resources (program prog) vbo vao)

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ =
  when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
    GLFW.setWindowShouldClose window True

gameLoop :: Window -> Resources -> IO ()
gameLoop window resources = do
  close <- GLFW.windowShouldClose window
  if close then do
    GLFW.destroyWindow window
    GLFW.terminate
  else do
    GL.clear [GL.ColorBuffer]
    draw resources
    GLFW.swapBuffers window
    GLFW.pollEvents
    gameLoop window resources

draw (Resources prog vbo vao) =
  withVAO vao $ do
   GL.drawArrays GL.Triangles 0 6

vertices = [v1,v2,v3,
            v2,v4,v5]

v1 :: Vertex3 GLfloat
v1 = GL.Vertex3 (-0.5) (-0.5) 0.0

v2 :: Vertex3 GLfloat
v2 = GL.Vertex3 0.5 0.5 0.0

v3 :: Vertex3 GLfloat
v3 = GL.Vertex3 (-0.5) 0.5 0.0

v4 :: Vertex3 GLfloat
v4 = GL.Vertex3 0.5 (-0.5) 0.0

v5 :: Vertex3 GLfloat
v5 = GL.Vertex3 0.0 (-0.7) 0.0

vert = "#version 330 core \
\ layout (location = 0) in vec3 v_position; \
\ void main(void) { \
\   gl_Position = vec4(v_position.x, v_position.y, v_position.z, 1.0); \
\}"

frag = "#version 330 core \
\ out vec4 color; \
\ void main(void) { \
\    color = vec4(0.9, 0.9, 1.0, 1.0f); \
\}"

