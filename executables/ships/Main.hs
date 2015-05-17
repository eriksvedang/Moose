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

type State = (VAOS.VAO, SHP.ShaderProgram, Ship)

data Ship = Ship { x :: Float
                 , y :: Float
                 , r :: Float
                 , ar :: Float } deriving (Show)

main :: IO ()
main = run ("Ships", 1600, 1200) setup draw tick onKey

setup :: GLFW.Window -> IO State
setup window = do
  GL.clearColor $= GL.Color4 0.9 0.95 0.95 1.0
  prog <- SHP.simpleShaderProgramBS vert frag
  vao <- VAOS.makeVAO $ do
    vbo <- BO.makeBuffer GL.ArrayBuffer shipVerts
    GL.currentProgram $= Just (SHP.program prog)
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    activateAttribute prog "v_position" 2
  return (vao, prog, initialShip)

initialShip = Ship (-7) (-5) 0.4 0.001

shipVerts :: [GL.GLfloat]
shipVerts = [-0.4, -0.3
           , 0.5,  0.0
           ,-0.4,  0.3]

keyCallback :: GLFW.KeyCallback
keyCallback window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = GLFW.setWindowShouldClose window True
keyCallback window _ _ _ _ = putStrLn "Invalid keyboard input."

mat4identity :: LM.M44 GLfloat
mat4identity = LM.identity

transform :: Float -> Float -> Float -> LM.M44 GLfloat
transform x y rot = LM.mkTransformation (LQ.axisAngle (V3 0 0 1) (realToFrac rot)) (V3 (realToFrac x) (realToFrac y) 0.0)

viewMatrix :: LM.M44 GLfloat
viewMatrix = LP.ortho (-8) 8 (-6) 6 (-1) 1

color :: GL.Vertex3 GLfloat
color = GL.Vertex3 1.0 0.8 0.2
             
draw :: State -> IO()
draw (vao, prog, (Ship x y r _)) = VAOS.withVAO vao $ do
  SHP.setUniform prog "u_color" color
  SHP.setUniform prog "u_transform" $ viewMatrix !*! (transform x y r)
  GL.drawArrays GL.Triangles 0 3

tick :: State -> State
tick s@(vao, prog, (Ship x y r ar)) = (vao, prog, newShip) where
  newR = r + ar
  newShip = Ship (x + 0.05 * cos(r)) (y + 0.05 * sin(r)) newR ar

onKey :: State -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> State
onKey s window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = s
onKey s@(vao, prog, ship@(Ship _ _ r _)) window GLFW.Key'A _ GLFW.KeyState'Pressed _ = (vao, prog, newShip) where
  newShip = ship { ar = 0.02 }
onKey s@(vao, prog, ship@(Ship _ _ r _)) window GLFW.Key'D _ GLFW.KeyState'Pressed _ = (vao, prog, newShip) where
  newShip = ship { ar = -0.02 }
onKey s@(vao, prog, ship@(Ship _ _ r _)) window GLFW.Key'A _ GLFW.KeyState'Released _ = (vao, prog, newShip) where
  newShip = ship { ar = 0 }
onKey s@(vao, prog, ship@(Ship _ _ r _)) window GLFW.Key'D _ GLFW.KeyState'Released _ = (vao, prog, newShip) where
  newShip = ship { ar = 0 }  
onKey s window _ _ _ _ = s

--GLFW.setWindowShouldClose window True

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
