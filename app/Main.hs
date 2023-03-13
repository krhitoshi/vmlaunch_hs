module Main (main) where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  dispatcher args
