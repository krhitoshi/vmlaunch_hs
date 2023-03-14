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
    vmDirPath <- getVmDirPath
    dirs <- listDirectory vmDirPath
    res <- mapM (\dir -> let path = vmDirPath </> dir in findVmxFile path) dirs
    let vmxFiles = catMaybes res
    putStr $ unlines $ let numbers = [0..] :: [Int]
        in zipWith (\n path -> show n ++ " - " ++ getVmNameFromVmxFilePath path) numbers vmxFiles

getVmDirPath :: IO FilePath
getVmDirPath = do
    home <- getEnv "HOME"
    return $ home </> "Virtual Machines.localized"

getVmNameFromVmxFilePath :: FilePath -> String
getVmNameFromVmxFilePath = dropExtension . takeFileName

findVmxFile :: FilePath -> IO (Maybe FilePath)
findVmxFile dir = do
    files <- getDirectoryContents dir
    case find isVmxFile files of
        Nothing -> return Nothing
        Just file -> return $ let path = dir </> file in Just path

isVmxFile :: FilePath -> Bool
isVmxFile path = takeExtension path == ".vmx"