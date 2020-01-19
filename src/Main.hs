module Main where

import Brick
import CommandLine

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = commandLine >> return ()
