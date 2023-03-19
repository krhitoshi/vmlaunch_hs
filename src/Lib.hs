module Lib
    ( dispatcher
    ) where

import System.IO
import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.Exit
-- import System.Directory
import Data.List (intercalate)
import System.Process

subcommands :: [String]
subcommands = ["list", "running", "start", "suspend", "snapshot", "snapshotList", "revert", "deleteSnapshot", "ssh", "startssh"]

dispatcher :: [String] -> IO ()
dispatcher ["list"] = showVmList
dispatcher ["running"] = showRunningVmList
dispatcher ("start":args) = startVm args
dispatcher ("suspend":args) = suspendVm args
dispatcher ("snapshot":args) = snapshotVm args
dispatcher ("snapshotList":args) = snapshotListVm args
dispatcher ("revert":args) = revertVm args
dispatcher ("deleteSnapshot":args) = deleteSnapshotVm args
dispatcher ("ssh":args) = sshVm args
dispatcher ("startssh":args) = do
    startVm args
    sshVm args
dispatcher (cmd:_) = do
    putStrLn $ "unknown command: " ++ cmd
    exitWith (ExitFailure 1)
dispatcher _ = do
    putStrLn "specify arguments"
    putStrLn $ "subcommands: " ++ intercalate ", " subcommands
    exitWith (ExitFailure 1)

showVmList :: IO ()
showVmList = do
    vmxFilePaths <- getVmxFilePaths
    putStr $ unlines $ let numbers = [0..] :: [Int]
        in zipWith (\n path -> show n ++ " - " ++ getVmNameFromVmxFilePath path) numbers vmxFilePaths

showRunningVmList :: IO ()
showRunningVmList = do
    execVmrun ["list"]

startVm :: [String] -> IO ()
startVm [numberString] = do
    vmxFilePath <- getVmxFilePath numberString
    execVmrun ["start", vmxFilePath, "gui"]
startVm _ = noVmNumberError

suspendVm :: [String] -> IO ()
suspendVm [numberString] = do
    vmxFilePath <- getVmxFilePath numberString
    execVmrun ["suspend", vmxFilePath]
suspendVm _ = noVmNumberError

snapshotVmBase :: String -> String -> IO ()
snapshotVmBase numberString operation = do
    vmxFilePath <- getVmxFilePath numberString
    execVmrun [operation, vmxFilePath, "vmlaunch-snapshot"]

snapshotVm :: [String] -> IO ()
snapshotVm [numberString] = snapshotVmBase numberString "snapshot"
snapshotVm _ = noVmNumberError

snapshotListVm :: [String] -> IO ()
snapshotListVm [numberString] = snapshotVmBase numberString "listSnapshots"
snapshotListVm _ = noVmNumberError

revertVm :: [String] -> IO ()
revertVm [numberString] = snapshotVmBase numberString "revertToSnapshot"
revertVm _ = noVmNumberError

deleteSnapshotVm :: [String] -> IO ()
deleteSnapshotVm [numberString] = snapshotVmBase numberString "deleteSnapshot"
deleteSnapshotVm _ = noVmNumberError

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
sshVm _ = noVmNumberError

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
getVmDirPath = fmap (</> "Virtual Machines.localized") $ getEnv "HOME"

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

noVmNumberError :: IO ()
noVmNumberError = do
    putStrLn "specify vm number"
    exitWith (ExitFailure 1)
