module Lib
    ( dispatcher
    ) where

import System.Environment
import System.FilePath

dispatcher :: [String] -> IO ()
dispatcher ["list"] = do
    home <- getEnv "HOME"
    let vmDir = home </> "Virtual Machines.localized"
    putStrLn vmDir
dispatcher (cmd:_) = putStrLn $ "unknown command: " ++ cmd
dispatcher _ = putStrLn $ "specify arguments"