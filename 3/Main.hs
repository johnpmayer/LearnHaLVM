
{-# OPTIONS -Wall #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- System Imports

import Control.Concurrent (
    threadDelay,
    forkIO)
import Control.Concurrent.MVar (
    newEmptyMVar)

-- import Data.Int (Int64)

import Foreign.Storable (peek, poke)

import Hypervisor.Basics (Xen)
import Hypervisor.Kernel (
    halvm_kernel_daemon)

import XenDevice.Console (dConsole)
import XenDevice.Disk (
    dDisks, 
    potentialDisks, 
    initializeDisk)
import XenDevice.Xenbus (dXenbus)

-- User Imports

import DataCache
import XUtils (xPrint, xError, xCatch)

incrTask :: DataCache -> Xen ()
incrTask dc = do
    task <- newEmptyMVar
    let pageAddr = 0
    ptr <- getPage task Exclusive pageAddr dc
    page <- peek ptr
    case page of
        IndexPage () -> 
            xError "expected table"
        TablePage i -> 
            poke ptr $ TablePage $ i + 1

observeTask :: DataCache -> Xen ()
observeTask dc = do
    xPrint "Observe Task"
    task <- newEmptyMVar
    let pageAddr = 0
    ptr <- getPage task Shared pageAddr dc
    xPrint $ "Got shared lock on ptr" ++ show ptr
    page <- xCatch (peek ptr)
    xPrint $ "Peeked the page from the pointer"
    case page of
        IndexPage () -> 
            xError "expected table"
        TablePage i -> 
            xPrint $ "Value stored in page is " ++ show i

idle :: Xen ()
idle = threadDelay 1000000 >> idle

start :: [String] -> Xen ()
start _args = do
    xPrint "Xen Machine is on"
    (diskName:_) <- potentialDisks
    (Just disk) <- initializeDisk diskName
    dc <- initialize disk 4
    xPrint "Data Cache created"
    _thread <- forkIO (observeTask dc)
    idle

main :: Xen ()
main = halvm_kernel_daemon [ dConsole, dDisks, dXenbus ] start

