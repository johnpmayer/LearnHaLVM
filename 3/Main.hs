
module Main where

-- System Imports

-- import Control.Concurrent (forkIO)

import Data.Int (Int64)

import Foreign.Storable (peek, poke)

import Hypervisor.Basics (Xen)
import Hypervisor.Kernel (
    halvm_kernel_daemon, 
    halvm_shutdown)
import Hypervisor.Memory (
    VPtr,
    allocPage,
    pageSize,
    setPageWritable)

import XenDevice.Console (dConsole, writeConsole)
import XenDevice.Disk (
    dDisks, 
    potentialDisks, 
    initializeDisk,
    writeBytes)
import XenDevice.Xenbus (dXenbus)

-- User Imports

import DataCache

xPrint :: String -> Xen ()
xPrint s = writeConsole s >> writeConsole "\n"

incrTask :: VPtr Int64 -> Xen ()
incrTask ptr = do
    i <- peek ptr
    poke ptr $ i + 1

observeTask :: VPtr Int64 -> Xen ()
observeTask ptr = do
    i <- peek ptr
    xPrint $ "Value stored in page is " ++ show i

setupPage :: Xen (VPtr Char)
setupPage = do
    ptr <- allocPage
    setPageWritable ptr True Nothing
    poke ptr 'X'
    return ptr

testWrite :: [String] -> VPtr Char -> Xen ()
testWrite []       _   = xPrint "No disks"
testWrite (name:_) ptr = do
    mDisk <- initializeDisk name
    case mDisk of
        Nothing     -> xPrint "Could not initialize"
        (Just disk) -> do
            result <- writeBytes disk 0 pageSize [ptr]
            xPrint $ "Write Result: " ++ show result
            return ()
            
start :: [String] -> Xen ()
start args = do
    xPrint "Xen Machine is on"
    page4K <- setupPage
    pDisks <- potentialDisks
    testWrite pDisks page4K
    halvm_shutdown

main :: Xen ()
main = halvm_kernel_daemon [ dConsole, dDisks, dXenbus ] start

