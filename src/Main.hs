{-# LANGUAGE OverloadedStrings #-}

module Main where

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

onError e message = putStrLn $ "ERROR!" ++ message

main :: IO ()
main = do
  success <- GLFW.init
  unless success $ putStrLn "Failed to init GLFW."
  GLFW.setErrorCallback (Just onError)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core);
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True);
  window <- GLFW.createWindow 1900 1200 "MOOSE" Nothing Nothing
  case window of
   (Just w) -> do r <- setup w
                  Just t <- GLFW.getTime
                  gameLoop w r t 0
   Nothing -> do putStrLn "Failed to create window."
                 
data RenderPass = RenderPass VAO ShaderProgram (RenderPass -> IO ()) BufferObject

stride steps = fromIntegral (sizeOf (undefined::GLfloat) * steps)

activateAttribute prog name floatCount = do
  let descriptor = VertexArrayDescriptor (fromIntegral floatCount) Float (stride floatCount) offset0
  enableAttrib prog name
  setAttrib prog name ToFloat descriptor

activateInstanced :: BufferObject -> GLuint -> IO ()
activateInstanced bufferData attributeLocation = do
  glEnableVertexAttribArray attributeLocation
  GL.bindBuffer ArrayBuffer $= Just bufferData
  glVertexAttribPointer
    attributeLocation
    1 -- components per vertex
    gl_FLOAT
    (fromBool False)
    0 -- stride
    nullPtr 
  glVertexAttribDivisor attributeLocation 1

setup :: Window -> IO [RenderPass]
setup window = do
  GLFW.makeContextCurrent (Just window)
  GLFW.setKeyCallback window (Just keyCallback)
  GL.clearColor $= Color4 0.9 0.95 0.95 1.0
  prog <- simpleShaderProgramBS vert frag
  offsets_vbo <- makeBuffer ArrayBuffer offsets
  vao <- makeVAO $ do
    vbo <- makeBuffer ArrayBuffer vertices
    GL.bindBuffer ArrayBuffer $= Just vbo
    GL.currentProgram $= Just (program prog)
    activateAttribute prog "v_position" 3
    activateInstanced offsets_vbo 1
  vao2 <- makeVAO $ do
    vbo <- makeBuffer ArrayBuffer vertices2
    GL.bindBuffer ArrayBuffer $= Just vbo
    GL.currentProgram $= Just (program prog)
    activateAttribute prog "v_position" 3
    activateInstanced offsets_vbo 1
  return [(RenderPass vao  prog rf1 offsets_vbo)
         ,(RenderPass vao2 prog rf2 offsets_vbo)
         ]

rf1 :: RenderPass -> IO ()
rf1 (RenderPass _ prog _ offsets_vbo) = do
  Just t <- GLFW.getTime
  let col :: Vertex3 GLfloat
      col = GL.Vertex3 0 (pulse t 0.5 0.75 5.0) 0.7
      off :: GLfloat
      off = -0.3
  setUniform prog "u_color" col
  setUniform prog "phaseT" ((realToFrac t) :: GLfloat)
  glDrawArraysInstanced gl_TRIANGLE_STRIP 0 4 quadCount

rf2 :: RenderPass -> IO ()
rf2 (RenderPass _ prog _ offsets_vbo) = do
  Just t <- GLFW.getTime
  let col :: Vertex3 GLfloat
      col = GL.Vertex3 1.0 (pulse t 0.5 0.75 5.0) 0.7
  setUniform prog "u_color" col
  setUniform prog "phaseT" ((realToFrac t) :: GLfloat)
  glDrawArraysInstanced gl_TRIANGLES 0 3 quadCount

quadCount :: CInt
quadCount = 10000

keyCallback :: GLFW.KeyCallback
keyCallback window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ =
  GLFW.setWindowShouldClose window True
keyCallback window _ _ _ _ = putStrLn "Invalid keyboard input."

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

pulse t low high freq =
  let diff = high - low
      half = diff / 2
  in  low + half + half * realToFrac (sin (t * freq))

draw :: RenderPass -> IO ()
draw r@(RenderPass vao prog renderFn offsets_vbo) =
  withVAO vao (renderFn r)

vertices :: [Vertex3 GLfloat]
vertices = [v3,v1,v2,v4]

vertices2 :: [Vertex3 GLfloat]
vertices2 = [v3,v1,v2]

offsets :: [GLfloat]
offsets = [-1.0, -0.9998 .. 1.0]

--concat $ [[x,y] | x <- [-1.0, -0.8.. 1.0]
--                , y <- [-1.0, -0.8.. 1.0]]

v1 :: Vertex3 GLfloat
v1 = GL.Vertex3 (-0.05) (-0.05) 0.0

v2 :: Vertex3 GLfloat
v2 = GL.Vertex3 0.05 0.05 0.0

v3 :: Vertex3 GLfloat
v3 = GL.Vertex3 (-0.05) 0.05 0.0

v4 :: Vertex3 GLfloat
v4 = GL.Vertex3 0.05 (-0.05) 0.0

vert = "#version 330 core \
\layout (location = 0) in vec3 v_position; \
\layout (location = 1) in float xoffset;\
\uniform vec3 u_color; \
\uniform float phaseT; \
\out vec3 f_color; \
\void main(void) { \
\ f_color = u_color; \
\ float x = v_position.x + xoffset;\
\ gl_Position = vec4(x, v_position.y + sin(x * 100 + phaseT) * cos(phaseT), v_position.z, 1.0); \
\}"

frag = "#version 330 core \
\out vec4 color; \
\in vec3 f_color; \
\void main(void) { \
\ color = vec4(f_color, 1.0f); \
\}"

