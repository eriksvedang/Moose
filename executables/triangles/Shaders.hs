{-# LANGUAGE OverloadedStrings #-}

module Shaders where

import Data.ByteString

vert :: ByteString
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

frag :: ByteString
frag = "#version 330 core \
\out vec4 color; \
\in vec3 f_color; \
\void main(void) { \
\ color = vec4(f_color, 1.0f); \
\}"

