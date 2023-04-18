module Lib
    ( dispatcher
    ) where

import System.IO
import System.Environment
import System.FilePath ( (</>), dropExtension, takeFileName )
import System.FilePath.Glob
import System.Exit
-- import System.Directory
import Data.List (intercalate)
import System.Process

subcommands :: [String]
subcommands = ["list", "running", "up", "suspend", "halt",
               "snapshot", "snapshotList", "revert", "deleteSnapshot",
               "ssh", "upssh"]

dispatcher :: [String] -> IO ()
dispatcher ["list"] = showVmList
dispatcher ["running"] = showRunningVmList
dispatcher ("up":args) = startVm args
dispatcher ("suspend":args) = suspendVm args
dispatcher ("halt":args) = stopSoftVm args
dispatcher ("snapshot":args) = snapshotVm args
dispatcher ("snapshotList":args) = snapshotListVm args
dispatcher ("revert":args) = revertVm args
dispatcher ("deleteSnapshot":args) = deleteSnapshotVm args
dispatcher ("ssh":args) = sshVm args
dispatcher ("upssh":args) = do
    startVm args
    sshVm args
dispatcher (cmd:_) = do
    putStrLn $ "unknown command: " ++ cmd
    exitWith (ExitFailure 1)
dispatcher _ = do
    putStrLn "specify arguments"
    putStrLn $ "subcommands: " ++ intercalate ", " subcommands
    exitWith (ExitFailure 1)

data VM = VM { vmId :: Int, vmName :: String,  vmxPath :: FilePath} deriving (Show)

showVmList :: IO ()
showVmList = do
    vms <- getVmxFilePaths >>= (return . getVms)
    putStr $ unlines $ map (\vm -> show (vmId vm) ++ " - " ++ vmName vm) vms

getVms :: [FilePath] -> [VM]
getVms vmxFilePaths = let numbers = [0..] :: [Int]
                      in zipWith (\n path -> VM { vmId = n, vmName = getVmNameFromVmxFilePath path, vmxPath = path }) numbers vmxFilePaths

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

stopSoftVm :: [String] -> IO ()
stopSoftVm [numberString] = do
    vmxFilePath <- getVmxFilePath numberString
    execVmrun ["stop", vmxFilePath, "soft"]
stopSoftVm _ = noVmNumberError

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
    let command = "ssh " ++ address
    putStrLn command
    _ <- system command
    return ()
sshVm _ = noVmNumberError

getVmxFilePath :: String -> IO FilePath
getVmxFilePath numberString = do
    let number = read numberString :: Int
    fmap (!! number) getVmxFilePaths

getVmxFilePaths :: IO [FilePath]
getVmxFilePaths = do
    getVmDirPath >>= globDir1 (compile "*/*.vmx")

getVmDirPath :: IO FilePath
getVmDirPath = fmap (</> "Virtual Machines.localized") $ getEnv "HOME"

getVmNameFromVmxFilePath :: FilePath -> String
getVmNameFromVmxFilePath = dropExtension . takeFileName

execVmrun :: [String] -> IO ()
execVmrun args = do
    putStrLn $ cmdLineStr "vmrun" args
    (_, Just hout, Just herr, _) <- createProcess (proc "vmrun" args){ std_out = CreatePipe, std_err = CreatePipe }
    out <- hGetContents hout
    err <- hGetContents herr
    putStrLn out
    putStrLn err
    return ()

cmdLineStr :: String -> [String] -> String
cmdLineStr cmd args = cmd ++ " " ++ (unwords . map quoteArg) args

quoteArg :: String -> String
quoteArg arg = if ' ' `elem` arg
               then ['"'] ++ arg ++ ['"']
               else arg

noVmNumberError :: IO ()
noVmNumberError = do
    putStrLn "specify vm number"
    exitWith (ExitFailure 1)
