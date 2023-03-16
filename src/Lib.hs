module Lib
    ( dispatcher
    ) where

import System.IO
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
dispatcher ("suspend":args) = suspendVm args
dispatcher ("ssh":args) = sshVm args
dispatcher ("startssh":args) = do
    startVm args
    sshVm args
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
    vmxFilePath <- getVmxFilePath numberString
    execVmrun ["start", vmxFilePath, "gui"]
startVm _ = do
    putStrLn "specify vm number"
    exitWith (ExitFailure 1)

suspendVm :: [String] -> IO ()
suspendVm [numberString] = do
    vmxFilePath <- getVmxFilePath numberString
    execVmrun ["suspend", vmxFilePath]
suspendVm _ = do
    putStrLn "specify vm number"
    exitWith (ExitFailure 1)

sshVm :: [String] -> IO ()
sshVm [numberString] = do
    vmxFilePath <- getVmxFilePath numberString
    result <- readProcess "vmrun" ["getGuestIPAddress", vmxFilePath, "-wait"] []
    -- Remove a new line character at the end
    let address = init result
    let command = "ssh " ++ address ++ ""
    putStrLn command
    _ <- system command
    return ()
sshVm _ = do
    putStrLn "specify vm number"
    exitWith (ExitFailure 1)

getVmxFilePath :: String -> IO FilePath
getVmxFilePath numberString = do
    let number = read numberString :: Int
    vmxFilePaths <- getVmxFilePaths
    let vmxFilePath = vmxFilePaths !! number
    return vmxFilePath

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

execVmrun :: [String] -> IO ()
execVmrun args = do
    (_, Just hout, Just herr, _) <- createProcess (proc "vmrun" args){ std_out = CreatePipe, std_err = CreatePipe }
    out <- hGetContents hout
    err <- hGetContents herr
    putStrLn out
    putStrLn err
    return ()