module Test where

import System.Environment
import System.Process

test = do
  setEnv "LOL" "1"
  system "echo $LOL"
