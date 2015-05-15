module Moose.GlHelp where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.BufferObjects (BufferObject)
import Graphics.GLUtil.BufferObjects (makeBuffer, offset0)
import Graphics.GLUtil.ShaderProgram (ShaderProgram, program, simpleShaderProgramBS, enableAttrib, setAttrib)
import Graphics.GLUtil (setUniform)
import Graphics.GLUtil.VertexArrayObjects (VAO, makeVAO, withVAO)
import Graphics.Rendering.OpenGL.Raw.ARB.DrawInstanced (glDrawArraysInstanced)
import Graphics.Rendering.OpenGL.Raw (gl_TRIANGLES, gl_LINE_STRIP, gl_TRIANGLE_STRIP)
import Graphics.Rendering.OpenGL.Raw.ARB.InstancedArrays (glVertexAttribDivisor)
import Graphics.Rendering.OpenGL.Raw -- (glVertexAttribPointer)

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable (sizeOf)

stride :: Num b => Int -> b
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


