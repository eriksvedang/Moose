module Main where

import Moose.Boilerplate

main :: IO ()
main = run defaultWindow setup

setup :: Window -> IO [RenderPass]
setup win = do
  return []

