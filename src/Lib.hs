module Lib
    ( dispatcher
    ) where

import System.Environment
import System.FilePath
import System.Exit
import System.Directory

dispatcher :: [String] -> IO ()
dispatcher ["list"] = do
    home <- getEnv "HOME"
    let vmDir = home </> "Virtual Machines.localized"
    dirs <- listDirectory vmDir
    -- print $ show dirs
    let dirs' = map ((</>) vmDir) dirs
    -- print $ show dirs'
    dirs'' <- mapM findVmxFiles dirs'
    let vmxFiles = concat dirs''
    putStr $ unlines vmxFiles
dispatcher (cmd:_) = do
    putStrLn $ "unknown command: " ++ cmd
    exitWith (ExitFailure 1)
dispatcher _ = do
    putStrLn "specify arguments"
    exitWith (ExitFailure 1)

findVmxFiles :: FilePath -> IO [FilePath]
findVmxFiles dir = do
    files <- getDirectoryContents dir
    return $ map ((</>) dir) (filter isVmxFile files)

isVmxFile :: FilePath -> Bool
isVmxFile path = drop ((length path) -4) path == ".vmx"