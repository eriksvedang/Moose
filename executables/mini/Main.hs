module Main where

import Moose.Boilerplate

main :: IO ()
main = run defaultWindow setup draw tick onKey

setup :: Window -> IO ()
setup win = return ()

draw :: () -> IO()
draw _ = return ()

tick :: () -> ()
tick _ = ()

onKey :: t -> t1 -> t2 -> t3 -> t4 -> t5 -> ()
onKey _ _ _ _ _ _ = ()

