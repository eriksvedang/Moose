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

getStride :: Num b => Int -> b
getStride steps = fromIntegral (sizeOf (undefined::GLfloat) * steps)

activateAttribute prog name floatCount = do
  let descriptor = VertexArrayDescriptor (fromIntegral floatCount) Float (getStride floatCount) offset0
  enableAttrib prog name
  setAttrib prog name ToFloat descriptor

-- | stride is the full size of the data for one "entity"
-- | divisor is usually 1 and is how many rendered objects should share one chunk of data
activateInstanced :: GLuint -> GLint -> Int -> Int -> GLuint -> IO ()
activateInstanced attributeLocation components strideSteps startPointer divisor = do
  glEnableVertexAttribArray attributeLocation
  glVertexAttribPointer
    attributeLocation
    components
    gl_FLOAT
    (fromBool False)
    (getStride strideSteps)
    (plusPtr nullPtr (sizeOf (undefined::GLfloat) * startPointer))
  glVertexAttribDivisor attributeLocation divisor


--nullPtr
