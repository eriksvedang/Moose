module Main where

import Moose.Boilerplate

main :: IO ()
main = run defaultWindow setup draw

setup :: Window -> IO ()
setup win = return ()

draw :: () -> IO()
draw _ = return ()
