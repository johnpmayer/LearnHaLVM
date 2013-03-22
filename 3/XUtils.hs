
module XUtils where

import Control.Concurrent (threadDelay)

import Control.Exception (SomeException)
import qualified Control.Exception as Ex

import Hypervisor.Basics (Xen)
import Hypervisor.Kernel (
    halvm_shutdown) 

import XenDevice.Console (writeConsole)

xPrint :: String -> Xen ()
xPrint s = writeConsole s >> writeConsole "\n"

xError :: String -> Xen ()
xError s = do
    xPrint $ "Error: " ++ s
    halvm_shutdown

xCatch :: Xen a -> Xen a
xCatch action = do 
    xPrint "Trying something dangerous?"
    Ex.catch action handler
    where
    handler :: SomeException -> Xen a
    handler e = do
        let s = "Caught runtime exception: " ++ show e
        xPrint s
        threadDelay 1000
        error s
