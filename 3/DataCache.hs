
{-# OPTIONS -Wall #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module DataCache where

import Control.Concurrent.STM (
    STM,
    TVar,
    newTVarIO,
    readTVar,
    writeTVar)

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Cache.LRU (LRU, newLRU)
import qualified Data.Cache.LRU as LRU
import           Data.Word (Word64)

import Hypervisor.Basics (Xen)
import Hypervisor.Memory (
    VPtr,
    allocPage,
    pageSize)

import           Queue (Queue)
import qualified Queue as Queue

import XenDevice.Disk (
    Disk,
    readBytes)

data LockType = Shared | Exclusive
data IOLatch = MVar ()

class PageCache t a where
  initialize :: Integer -> Xen t
  getPage :: LockType -> Word64 -> t -> Xen (Either IOLatch (VPtr a))
  releasePage :: Word64 -> t -> Xen ()

type PageAddr = Word64

data Page = IndexPage () | TablePage ()

type TaskID = ()

type TaskWaiting = (TaskID, LockType, Int)

data Status = Kernel PageAddr 
            | Free (VPtr Page) 
            | HeldShared (Set TaskID) (VPtr Page)
            | HeldExclusive TaskID (VPtr Page)

data DataCacheElem = DCE Status (Queue TaskWaiting)

-- nice place for a GADT to only read pages in 'Kernel' and only write pages in 'Free'
data DiskOperation = Read PageAddr DataCacheElem | Write PageAddr DataCacheElem

data DataCache = DC Disk (TVar (LRU Word64 DataCacheElem)) (TVar (Queue DiskOperation))

initializeDC :: Integer -> Disk -> Xen DataCache
initializeDC size disk = do
    alru <- newTVarIO . newLRU . Just $ size
    diskOps <- newTVarIO $ Queue.empty
    return $ DC disk alru diskOps

getPageDC :: TaskID -> LockType -> PageAddr -> DataCache -> STM (Maybe (VPtr Page))
getPageDC task lock page (DC _ tlru tDiskOps) = do
    lru <- readTVar tlru
    case LRU.lookup page lru of
        (_, Nothing)     -> do
            let dcElem = DCE (Kernel page) (Queue.enqueue (task, lock, 0) Queue.empty)
            let lru' = LRU.insert page dcElem lru
            writeTVar tlru lru'
            diskOps <- readTVar tDiskOps
            let diskOps' = Queue.enqueue (Read page dcElem) diskOps
            writeTVar tDiskOps diskOps'
            return Nothing
        (_, Just (DCE status twq)) -> case status of
            (Kernel _) -> do
                let twq' = Queue.enqueue (task, lock, 0) twq
                let dcElem = DCE status twq'
                let lru' = LRU.insert page dcElem lru
                writeTVar tlru lru'
                return Nothing
            (Free ptr) -> case lock of
                Shared -> do
                    let status' = HeldShared (Set.singleton task) ptr
                    let dcElem = DCE status' twq
                    let lru' = LRU.insert page dcElem lru
                    writeTVar tlru lru'
                    return $ Just ptr
                Exclusive -> do
                    let status' = HeldExclusive task ptr
                    let dcElem = DCE status' twq
                    let lru' = LRU.insert page dcElem lru
                    writeTVar tlru lru'
                    return $ Just ptr
            (HeldShared _ts _ptr) -> undefined
            (HeldExclusive _t _) -> undefined

releasePageDC :: DataCache -> Word64 -> Xen ()
releasePageDC = undefined

{-
instance PageCache DataCache Page where
initialize = initializeDC
  getPage = getPageDC
  releasePage = releasePageDC
  -}
