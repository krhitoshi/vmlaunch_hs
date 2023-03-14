module Lib
    ( dispatcher
    ) where

import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.Exit
-- import System.Directory
-- import Data.List (find)
import System.Process

dispatcher :: [String] -> IO ()
dispatcher ["list"] = showVmList
dispatcher ("start":args) = startVm args
dispatcher (cmd:_) = do
    putStrLn $ "unknown command: " ++ cmd
    exitWith (ExitFailure 1)
dispatcher _ = do
    putStrLn "specify arguments"
    exitWith (ExitFailure 1)

showVmList :: IO ()
showVmList = do
    vmxFilePaths <- getVmxFilePaths
    putStr $ unlines $ let numbers = [0..] :: [Int]
        in zipWith (\n path -> show n ++ " - " ++ getVmNameFromVmxFilePath path) numbers vmxFilePaths

startVm :: [String] -> IO ()
startVm [numberString] = do
    let number = read numberString :: Int
    vmxFilePaths <- getVmxFilePaths
    let vmxFilePath = vmxFilePaths !! number
    let command = "vmrun start '" ++ vmxFilePath ++ "' gui"
    putStrLn command
    _ <- system command
    return ()
startVm _ = do
    putStrLn "specify vm number"
    exitWith (ExitFailure 1)

getVmxFilePaths :: IO [FilePath]
getVmxFilePaths = do
    vmDirPath <- getVmDirPath
    paths <- globDir1 (compile "*/*.vmx") vmDirPath
    return paths

getVmDirPath :: IO FilePath
getVmDirPath = do
    home <- getEnv "HOME"
    return $ home </> "Virtual Machines.localized"

getVmNameFromVmxFilePath :: FilePath -> String
getVmNameFromVmxFilePath = dropExtension . takeFileName
