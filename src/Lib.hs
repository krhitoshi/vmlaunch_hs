module Lib
    ( dispatcher
    ) where

import System.Environment
import System.FilePath
import System.Exit

dispatcher :: [String] -> IO ()
dispatcher ["list"] = do
    home <- getEnv "HOME"
    let vmDir = home </> "Virtual Machines.localized"
    putStrLn vmDir
dispatcher (cmd:_) = do
    putStrLn $ "unknown command: " ++ cmd
    exitWith (ExitFailure 1)
dispatcher _ = do
    putStrLn "specify arguments"
    exitWith (ExitFailure 1)
