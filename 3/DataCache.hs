
{-# OPTIONS -Wall #-}

{-# LANGUAGE GADTs, TypeSynonymInstances #-}

module DataCache where

import Control.Concurrent (
    forkIO)
import Control.Concurrent.MVar (
    MVar,
    putMVar,
    takeMVar)

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

import XUtils (xPrint)

data LockType = Shared | Exclusive
data IOLatch = MVar ()

{-
data Index
data Table
-}

data Page = IndexPage () | TablePage Int
{-
data Page a where
    IndexPage :: () -> Page Index
    TablePage :: Int -> Page Table
-}
instance Storable Page where
    alignment = undefined
    sizeOf = undefined

type TaskID = MVar (VPtr Page)
type PageAddr = Word64

type TaskWaiting = (TaskID, LockType, Int)

data Status = Kernel PageAddr 
            | Free (VPtr Page) 
            | HeldShared (Set TaskID) (VPtr Page)
            | HeldExclusive TaskID (VPtr Page)

data DataCacheElem = DCE Status (Queue TaskWaiting)

-- nice place for a GADT to only read pages in 'Kernel' and only write pages in 'Free'
data DiskOperation = Read PageAddr | Write PageAddr deriving (Show)

data DataCache = DC Disk (TVar (LRU Word64 DataCacheElem)) (TVar (Queue DiskOperation))

initialize :: Disk -> Integer -> Xen DataCache
initialize disk size = do
    alru <- newTVarIO . newLRU . Just $ size
    diskOps <- newTVarIO $ Queue.empty
    return $ DC disk alru diskOps

makeStatus :: TaskID -> LockType -> VPtr Page -> Status
makeStatus task Shared    = HeldShared (Set.singleton task) 
makeStatus task Exclusive = HeldExclusive task

-- TODO rename, since this is actually updating the lru
nextTask :: PageAddr -> VPtr Page -> TVar (LRU PageAddr DataCacheElem) -> STM (Maybe TaskID)
nextTask page ptr tlru = do
    lru <- readTVar tlru
    let (_, mdce) = LRU.lookup page lru
    case mdce of
        Nothing -> return Nothing
        Just (DCE _status twq) -> do
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
        (Just diskOp) -> do
            case diskOp of
                Read page -> do
                    ptr <- allocPage
                    _result <- readBytes disk page pageSize [ptr]
                    -- go to the cache and find out who's next
                    mTask <- atomically (nextTask page ptr tlru)
                    -- release to that thread
                    case mTask of 
                        Nothing -> return ()
                        (Just task) -> putMVar task ptr
                Write _page -> undefined

getPage :: TaskID -> LockType -> PageAddr -> DataCache -> Xen (VPtr Page)
getPage task lock page dc = do
    mVPtr <- atomically (reservePageDC task lock page dc)
    case mVPtr of
        (Just ptr) -> return ptr
        Nothing    -> do 
            xPrint "Page not in cache, forking disk read thread"
            _thread <- forkIO (doDiskOp dc)
            takeMVar task

reservePageDC :: TaskID -> LockType -> PageAddr -> DataCache -> STM (Maybe (VPtr Page))
reservePageDC task lock page (DC _ tlru tDiskOps) = do
    lru <- readTVar tlru
    case LRU.lookup page lru of
        (_, Nothing)     -> do
            let dcElem = DCE (Kernel page) (Queue.enqueue (task, lock, 0) Queue.empty)
            let lru' = LRU.insert page dcElem lru
            writeTVar tlru lru'
            let diskOp = Read page
            diskOps <- readTVar tDiskOps
            let diskOps' = Queue.enqueue diskOp diskOps
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

releasePage :: DataCache -> Word64 -> Xen ()
releasePage = undefined

