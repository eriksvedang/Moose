{-# LANGUAGE OverloadedStrings #-}

module Main where

import Moose.Boilerplate (run)
import Moose.GlHelp (activateAttribute)
import Graphics.Rendering.OpenGL (($=))
import Data.ByteString (ByteString)
import Graphics.Rendering.OpenGL (GLfloat)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Matrix ((!*!))

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil.BufferObjects as BO
import qualified Graphics.GLUtil.VertexArrayObjects as VAOS
import qualified Graphics.GLUtil.ShaderProgram as SHP
import qualified Graphics.GLUtil.Linear as UL
import qualified Linear.Matrix as LM
import qualified Linear.Quaternion as LQ
import qualified Linear.Projection as LP

main :: IO ()
main = run ("Ships", 1600, 1200) setup draw

setup :: GLFW.Window -> IO (VAOS.VAO, SHP.ShaderProgram)
setup window = do
  GLFW.makeContextCurrent (Just window)
  GLFW.setKeyCallback window (Just keyCallback)
  GL.clearColor $= GL.Color4 0.9 0.95 0.95 1.0
  prog <- SHP.simpleShaderProgramBS vert frag
  vao <- VAOS.makeVAO $ do
    vbo <- BO.makeBuffer GL.ArrayBuffer shipVerts
    GL.currentProgram $= Just (SHP.program prog)
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    activateAttribute prog "v_position" 2
  return (vao, prog)

shipVerts :: [GL.GLfloat]
shipVerts = [-0.4, -0.3
           , 0.5,  0.0
           ,-0.4,  0.3]

mat4identity :: LM.M44 GLfloat
mat4identity = LM.identity

transform :: GLfloat -> LM.M44 GLfloat
transform rot = LM.mkTransformation (LQ.axisAngle (V3 0 0 1) rot) (V3 (-0.3) 0.0 0.0)
            
draw :: (VAOS.VAO, SHP.ShaderProgram) -> IO()
draw (vao, prog) = VAOS.withVAO vao $ do
  Just t <- GLFW.getTime
  let col :: GL.Vertex3 GLfloat
      col = GL.Vertex3 1.0 0.1 (pulse t 0.5 0.75 5.0)
  SHP.setUniform prog "u_color" col
  let viewMatrix :: LM.M44 GLfloat
      viewMatrix = LP.ortho (-8) 8 (-6) 6 (-1) 1
  SHP.setUniform prog "u_transform" $ viewMatrix !*! (transform (realToFrac t))
  GL.drawArrays GL.Triangles 0 3

keyCallback :: GLFW.KeyCallback
keyCallback window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ =
  GLFW.setWindowShouldClose window True
keyCallback window _ _ _ _ = putStrLn "Invalid keyboard input."

vert :: ByteString
vert = "#version 330 core \
\layout (location = 0) in vec2 v_position; \
\uniform vec3 u_color; \
\uniform mat4 u_transform; \
\out vec3 f_color; \
\void main(void) { \
\ f_color = u_color; \
\ gl_Position = u_transform * vec4(v_position.xy, 1.0, 1.0); \
\}"

frag :: ByteString
frag = "#version 330 core \
\out vec4 color; \
\in vec3 f_color; \
\void main(void) { \
\ color = vec4(f_color, 1.0f); \
\}"

pulse t low high freq =
  let diff = high - low
      half = diff / 2
  in  low + half + half * realToFrac (sin (t * freq))