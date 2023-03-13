module Lib
    ( dispatcher
    ) where

dispatcher :: [String] -> IO ()
dispatcher ["list"] = putStrLn "list"
dispatcher (cmd:_) = putStrLn $ "unknown command: " ++ cmd
dispatcher _ = putStrLn $ "specify arguments"