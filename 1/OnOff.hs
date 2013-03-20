
module Main where

import Hypervisor.Debug
import Hypervisor.Kernel

import XenDevice.Console
import XenDevice.Disk
import XenDevice.Xenbus

main :: IO ()
main = halvm_kernel_daemon [ dConsole, dDisks, dXenbus ] start

start :: [String] -> IO ()
start args = do
    writeDebugConsole "Xen Machine is on"
    halvm_shutdown

