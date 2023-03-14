module Lib
    ( dispatcher
    ) where

import System.Environment
import System.FilePath
import System.Exit
import System.Directory
import Data.List (find)
import Data.Maybe

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
    res <- mapM (\dir -> let path = vmDir </> dir in findVmxFile path) dirs
    let vmxFiles = catMaybes res
    putStr $ unlines $ let numbers = [0..] :: [Int]
        in zipWith (\n path -> show n ++ " - " ++ path) numbers vmxFiles

findVmxFile :: FilePath -> IO (Maybe FilePath)
findVmxFile dir = do
    files <- getDirectoryContents dir
    case find isVmxFile files of
        Nothing -> return Nothing
        Just file -> return $ let path = dir </> file in Just path

isVmxFile :: FilePath -> Bool
isVmxFile path = takeExtension path == ".vmx"