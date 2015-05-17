{-# LANGUAGE OverloadedStrings #-}

module Main where

import Moose.Boilerplate (run)
import Moose.GlHelp (activateAttribute)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil.BufferObjects as BO
import Graphics.GLUtil.VertexArrayObjects as VAOS
import Data.ByteString (ByteString)
import Graphics.GLUtil.ShaderProgram as SHP

main :: IO ()
main = run ("Ships", 800, 600) setup draw

setup :: GLFW.Window -> IO ()
setup window = do
  GLFW.makeContextCurrent (Just window)
  GLFW.setKeyCallback window (Just keyCallback)
  GL.clearColor $= GL.Color4 0.9 0.95 0.95 1.0
  prog <- SHP.simpleShaderProgramBS vert frag
  vao <- VAOS.makeVAO $ do
    vbo <- BO.makeBuffer GL.ArrayBuffer vertices
    GL.currentProgram $= Just (SHP.program prog)
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    activateAttribute prog "v_position" 3
  return ()

vertices :: [GL.GLfloat]
vertices = [-0.9, -0.3
           , 0.9,  0.0
           ,-0.9,  0.3]
  
draw :: () -> IO()
draw _ = return ()

keyCallback :: GLFW.KeyCallback
keyCallback window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ =
  GLFW.setWindowShouldClose window True
keyCallback window _ _ _ _ = putStrLn "Invalid keyboard input."

vert :: ByteString
vert = "#version 330 core \
\layout (location = 0) in vec3 v_position; \
\uniform vec3 u_color = vec3(1,1,0); \
\out vec3 f_color; \
\void main(void) { \
\ f_color = u_color; \
\ gl_Position = vec4(v_position.xyz, 1.0); \
\}"

frag :: ByteString
frag = "#version 330 core \
\out vec4 color; \
\in vec3 f_color; \
\void main(void) { \
\ color = vec4(f_color, 1.0f); \
\}"

