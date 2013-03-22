
{-# OPTIONS -Wall #-}

module DataCache where

import Control.Concurrent.MVar (
    MVar,
    putMVar)

import Control.Concurrent.STM (
    STM,
    atomically,
    TVar,
    newTVarIO,
    readTVar,
    writeTVar)

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Cache.LRU (LRU, newLRU)
import qualified Data.Cache.LRU as LRU
import           Data.Word (Word64)

import Foreign.Storable

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
data Page = IndexPage () | TablePage Int
type TaskID = MVar (VPtr Page)
type PageAddr = Word64

instance Storable Page where
    alignment = undefined
    sizeOf = undefined

class PageCache t where
  initialize :: Integer -> Disk -> Xen t
  getPage :: TaskID -> LockType -> PageAddr -> t 
          -> Xen (VPtr Page)
  releasePage :: TaskID -> PageAddr -> t -> Xen ()

type TaskWaiting = (TaskID, LockType, Int)

data Status = Kernel PageAddr 
            | Free (VPtr Page) 
            | HeldShared (Set TaskID) (VPtr Page)
            | HeldExclusive TaskID (VPtr Page)

data DataCacheElem = DCE Status (Queue TaskWaiting)

-- nice place for a GADT to only read pages in 'Kernel' and only write pages in 'Free'
data DiskOperation = Read PageAddr DataCacheElem | Write PageAddr DataCacheElem

data DataCache = DC Disk (TVar (LRU Word64 DataCacheElem)) (TVar (Queue (TaskID, DiskOperation)))

initializeDC :: Integer -> Disk -> Xen DataCache
initializeDC size disk = do
    alru <- newTVarIO . newLRU . Just $ size
    diskOps <- newTVarIO $ Queue.empty
    return $ DC disk alru diskOps

makeStatus :: TaskID -> LockType -> VPtr Page -> Status
makeStatus task Shared    = HeldShared (Set.singleton task) 
makeStatus task Exclusive = HeldExclusive task

nextTask :: PageAddr -> VPtr Page -> DataCache -> STM (Maybe TaskID)
nextTask page ptr (DC _ tlru _) = do
    lru <- readTVar tlru
    let (_, mdce) = LRU.lookup page lru
    case mdce of
        Nothing -> return Nothing
        Just (DCE status twq) -> do
            let (twq', mt) = Queue.dequeue twq
            case mt of
                Nothing -> return Nothing
                (Just (task, lock, _)) -> do
                    let status' = makeStatus task lock ptr
                    let dce = DCE status' twq'
                    let lru' = LRU.insert page dce lru
                    writeTVar tlru lru'
                    return $ Just task

doDiskOp :: DataCache -> Xen ()
doDiskOp (DC disk tlru tDiskOps) = do
    mDiskOp <- atomically $ do
        q <- readTVar tDiskOps
        let (q', m) = Queue.dequeue q
        writeTVar tDiskOps q'
        return m
    case mDiskOp of
        Nothing -> return ()
        (Just (task, diskOp)) -> do
            case diskOp of
                Read page (DCE status twq) -> do
                    ptr <- allocPage
                    _result <- readBytes disk page pageSize [ptr]
                    -- check the result
                    putMVar task ptr
                    -- go to the cache and find out who's next
                    -- release to that thread
                Write page dcElem -> undefined

getPageDC :: TaskID -> LockType -> PageAddr -> DataCache -> Xen (VPtr Page)
getPageDC task lock page dc@(DC _ tlru tDiskOps) = do
    mVPtr <- atomically (reservePageDC task lock page dc)
    case mVPtr of
        (Just ptr) -> return ptr
        Nothing    -> undefined

reservePageDC :: TaskID -> LockType -> PageAddr -> DataCache -> STM (Maybe (VPtr Page))
reservePageDC task lock page (DC _ tlru tDiskOps) = do
    lru <- readTVar tlru
    case LRU.lookup page lru of
        (_, Nothing)     -> do
            let dcElem = DCE (Kernel page) (Queue.enqueue (task, lock, 0) Queue.empty)
            let lru' = LRU.insert page dcElem lru
            writeTVar tlru lru'
            let diskOp = Read page dcElem
            diskOps <- readTVar tDiskOps
            let diskOps' = Queue.enqueue (task, diskOp) diskOps
            writeTVar tDiskOps diskOps'
            return Nothing
        (_, Just (DCE status twq)) -> case status of
            (Kernel _) -> do
                -- page is reserved in cache, but kernel 
                -- is still reading from disk
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

instance PageCache DataCache where
  initialize = initializeDC
  getPage = getPageDC
  releasePage = undefined
