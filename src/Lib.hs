module Lib
    ( dispatcher
    ) where

import System.Environment
import System.FilePath
import System.Exit
import System.Directory

dispatcher :: [String] -> IO ()
dispatcher ["list"] = showVmList
dispatcher (cmd:_) = do
    putStrLn $ "unknown command: " ++ cmd
    exitWith (ExitFailure 1)
dispatcher _ = do
    putStrLn "specify arguments"
    exitWith (ExitFailure 1)

showVmList :: IO ()
showVmList = do
    home <- getEnv "HOME"
    let vmDir = home </> "Virtual Machines.localized"
    dirs <- listDirectory vmDir
    files <- mapM (\dir -> let path = vmDir </> dir in findVmxFiles path) dirs
    let vmxFiles = concat files
    putStr $ unlines $ let numbers = [0..] :: [Int]
        in zipWith (\n path -> show n ++ " - " ++ path) numbers vmxFiles

findVmxFiles :: FilePath -> IO [FilePath]
findVmxFiles dir = do
    files <- getDirectoryContents dir
    return $ map ((</>) dir) (filter isVmxFile files)

isVmxFile :: FilePath -> Bool
isVmxFile path = drop ((length path) -4) path == ".vmx"