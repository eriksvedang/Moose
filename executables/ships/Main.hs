{-# LANGUAGE OverloadedStrings #-}

module Main where

import Moose.Boilerplate (run)
import Moose.GlHelp (activateAttribute, activateInstanced)
import Graphics.Rendering.OpenGL (($=))
import Data.ByteString (ByteString)
import Graphics.Rendering.OpenGL (GLfloat)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Matrix ((!*!))
import Control.Monad (replicateM)
import Data.List (find)
import Graphics.Rendering.OpenGL.Raw.ARB.DrawInstanced (glDrawArraysInstanced)
import Graphics.Rendering.OpenGL.Raw (gl_TRIANGLES, gl_LINE_STRIP, gl_TRIANGLE_STRIP)
import Graphics.Rendering.OpenGL.Raw.ARB.InstancedArrays (glVertexAttribDivisor)
import qualified Control.Monad.State as S
import qualified System.Random as R
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil.BufferObjects as BO
import qualified Graphics.GLUtil.VertexArrayObjects as VAOS
import qualified Graphics.GLUtil.ShaderProgram as SHP
import qualified Graphics.GLUtil.Linear as UL
import qualified Linear.Matrix as LM
import qualified Linear.Quaternion as LQ
import qualified Linear.Projection as LP
import qualified Data.Vector.Storable as SV

data State = State { _vao :: VAOS.VAO
                   , _prog :: SHP.ShaderProgram
                   , _window :: GLFW.Window
                   , _ships :: [Ship]
                   , _instanceBuffer :: GL.BufferObject
                   }

data Ship = Ship { _x :: Float
                 , _y :: Float
                 , _rgb :: (Float, Float, Float)
                 , _r :: Float
                 , _ar :: Float
                 } deriving (Show)

main :: IO ()
main = run ("Ships", 1600, 1200) setup draw tick onKey

setup :: GLFW.Window -> IO State
setup window = do
  GL.clearColor $= GL.Color4 0.9 0.95 0.95 1.0
  prog <- SHP.simpleShaderProgramBS vert frag
  instanceBuffer <- BO.makeBuffer GL.ArrayBuffer ([] :: [GLfloat]) -- will fill it later
  g <- R.newStdGen
  let enemies = mkEnemies g
  vao <- VAOS.makeVAO $ do
    vbo <- BO.makeBuffer GL.ArrayBuffer shipVerts
    GL.currentProgram $= Just (SHP.program prog)
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    activateAttribute prog "v_position" 2
    GL.bindBuffer GL.ArrayBuffer $= Just instanceBuffer
    activateInstanced 1 2 6 0 1 -- loc, components, stride, start, divisor
    activateInstanced 2 3 6 2 1
    activateInstanced 3 1 6 5 1
  return $ State vao prog window (initialShip : enemies) instanceBuffer

initialShip :: Ship
initialShip = Ship { _x = (-700)
                   , _y = 400
                   , _rgb = (1, 0, 0.5)
                   , _r = 0.4
                   , _ar = 0.001
                   }

mkEnemies :: R.StdGen -> [Ship]
mkEnemies g = S.evalState (replicateM 1000 mkEnemy) g

mkEnemy :: S.State R.StdGen Ship
mkEnemy = do
  x <- getRand (-500.0, 500.0)
  y <- getRand (-500.0, 500.0)
  angleSpeed <- getRand (-0.1, 0.1)
  return (Ship x y (0.1, 0.7, 0.5) 0 angleSpeed)

shipToData :: Ship -> [GLfloat]
shipToData (Ship x y (r,g,b) rr _) =
  [realToFrac x,
   realToFrac y,
   realToFrac r,
   realToFrac g,
   realToFrac b,
   realToFrac rr]

shipVerts :: [GL.GLfloat]
shipVerts = [-20, -15
            , 25,  00
            ,-20,  15]

mat4identity :: LM.M44 GLfloat
mat4identity = LM.identity

transform :: Float -> Float -> Float -> LM.M44 GLfloat
transform x y rot = LM.mkTransformation (LQ.axisAngle (V3 0 0 1) (realToFrac rot)) (V3 (realToFrac x) (realToFrac y) 0.0)

viewMatrix :: Float -> Float -> LM.M44 GLfloat
viewMatrix w h = LP.ortho (realToFrac (-w/2)) (realToFrac (w/2)) (realToFrac (-h/2)) (realToFrac (h/2)) (-1) 1

color :: GL.Vertex3 GLfloat
color = GL.Vertex3 1.0 0.8 0.2
             
draw :: State -> IO ()
draw state =
  let vao = _vao state
      prog = _prog state
      window = _window state
      instanceBuffer = _instanceBuffer state
      ships = _ships state
      c = length ships
  in VAOS.withVAO vao $ do
  (w, h) <- GLFW.getWindowSize window
  SHP.setUniform prog "u_view" $ (viewMatrix (fromIntegral w) (fromIntegral h))
  GL.bindBuffer GL.ArrayBuffer $= Just instanceBuffer
  SV.unsafeWith (shipsInstanceData ships) $ (\p -> GL.bufferData GL.ArrayBuffer $= (fromIntegral $ c*5*8, p, GL.StreamDraw))
  glDrawArraysInstanced gl_TRIANGLES 0 3 (fromIntegral c)

shipsInstanceData :: [Ship] -> SV.Vector GLfloat
shipsInstanceData ships = SV.fromList $ concatMap shipToData ships

tick :: State -> State
tick = checkCollisions . moveShips

moveShips :: State -> State
moveShips state = state { _ships = fmap (tickShip 1.0) (_ships state) }

tickShip :: Float -> Ship -> Ship
tickShip dt ship =
  let (Ship x y rgb r ar) = ship
      newR = r + ar
  in Ship (x + 4 * dt * cos(r)) (y - 4 * dt * sin(r)) rgb newR ar

checkCollisions :: State -> State
checkCollisions state =
  let ships = _ships state
      newShips = reverseIfColliding ships
  in state { _ships = newShips }

reverseIfColliding :: [Ship] -> [Ship]
reverseIfColliding [] = []
reverseIfColliding (ship : ships) = newShip : (reverseIfColliding ships) where
  newShip = case find (doesCollide ship) ships of
    Nothing -> ship
    Just _ -> tickShip 10.0 $ ship { _r = (_r ship) + 3.1416 }

doesCollide :: Ship -> Ship -> Bool
doesCollide a b = (dx * dx) + (dy * dy) < d * d where
  dx = (_x a) - (_x b)
  dy = (_y a) - (_y b)
  d  = 30

onKey :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> State -> State
onKey window key _ keyState _ state = state { _ships = newShips } where
  ships = _ships state
  ship = head $ ships
  newShip = case keyState of
             GLFW.KeyState'Pressed -> case key of
                                       GLFW.Key'A -> setRotationSpeed ship (-0.06)
                                       GLFW.Key'D -> setRotationSpeed ship ( 0.06)
                                       _ -> ship
             GLFW.KeyState'Released -> setRotationSpeed ship 0
             _ -> ship
  newShips = newShip : (tail ships)
  
setRotationSpeed :: Ship -> Float -> Ship
setRotationSpeed ship newAngularRotation = ship { _ar = newAngularRotation }

vert :: ByteString
vert = "#version 330 core \
\layout (location = 0) in vec2 v_position; \
\layout (location = 1) in vec2 v_worldPos; \
\layout (location = 2) in vec3 v_color; \
\layout (location = 3) in float v_rotation; \
\uniform mat4 u_view; \
\out vec3 f_color; \
\void main(void) { \
\ f_color = v_color; \
\ mat4 rotationMatrix = mat4( cos( v_rotation ), -sin( v_rotation ), 0.0, 0.0, \
\                             sin( v_rotation ),  cos( v_rotation ), 0.0, 0.0, \
\                                           0.0,                0.0, 1.0, 0.0, \
\                                           0.0,                0.0, 0.0, 1.0 ); \
\ gl_Position = u_view * (rotationMatrix * (vec4(v_position.xy, 1.0, 1.0)) + vec4(v_worldPos.xy, 0.0, 0.0)); \
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

getRand :: (Float, Float) -> S.State R.StdGen Float
getRand range = 
    do g <- S.get
       let (r, g') = R.randomR range g
       S.put g'
       return r
