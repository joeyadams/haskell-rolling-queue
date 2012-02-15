-- |
-- Module:       Data.STM.RollingQueue
-- Copyright:    (c) Joseph Adams 2012
-- License:      BSD3
-- Maintainer:   joeyadams3.14159@gmail.com
-- Portability:  Requires STM
--
-- This module is usually imported qualified:
--
-- >import Data.STM.RollingQueue (RollingQueue)
-- >import qualified Data.STM.RollingQueue as RQ
{-# LANGUAGE DeriveDataTypeable #-}
module Data.STM.RollingQueue (
    RollingQueue,
    new,
    newIO,
    write,
    read,
    tryRead,
    isEmpty,
    setLimit,
) where

import Prelude hiding (read)

import Control.Concurrent.STM
import Data.Typeable (Typeable)

-- | A 'RollingQueue' is a bounded FIFO channel.  When the size limit is
-- exceeded, older entries are discarded to make way for newer entries.
--
-- Note: if the size limit is less than @1@, 'write' will have no effect, and
-- 'read' will always 'retry'.
data RollingQueue a = RQ (TVar (ReadEnd a)) (TVar (WriteEnd a))
    deriving Typeable

instance Eq (RollingQueue a) where
    (==) (RQ r1 _) (RQ r2 _) = r1 == r2

data ReadEnd a = ReadEnd   !(TCell a)   -- Pointer to next item in the stream
                           !Int         -- Number of reads since we last synced
                                        -- with the writer
                           !Int         -- Number of log entries discarded
                                        -- since the last read

data WriteEnd a = WriteEnd !(TCell a)   -- Pointer to the hole (which is a TNil)
                           !Int         -- Number of writes since we last
                                        -- synced with the reader
                           !Int         -- Size limit

type TCell a = TVar (TList a)
data TList a = TNil | TCons a (TCell a)


-- | Create a new, empty 'RollingQueue', with the given size limit.
--
-- To change the size limit later, use 'setLimit'.
new :: Int -> STM (RollingQueue a)
new limit = undefined

{- |
@IO@ variant of 'new'.  This is useful for creating top-level
'RollingQueue's using 'System.IO.Unsafe.unsafePerformIO', because performing
'atomically' inside a pure computation is extremely dangerous (can lead to
'Control.Exception.NestedAtomically' errors and even segfaults, see GHC ticket
#5866).

Example:

@
logQueue :: 'RollingQueue' LogEntry
logQueue = 'System.IO.Unsafe.unsafePerformIO' (RQ.'newIO' 1000)
\{\-\# NOINLINE logQueue \#\-\}
@
-}
newIO :: Int -> IO (RollingQueue a)
newIO limit = undefined

-- | Write an entry to the rolling queue.  If the queue is full, discard the
-- oldest entry.
--
-- There is no @tryWrite@, because 'write' never retries.
write :: RollingQueue a -> a -> STM ()
write = undefined

-- | Read the next entry from the 'RollingQueue'.  'retry' if the queue is
-- empty.
--
-- The 'Int' is the number of entries discarded since the last read operation
-- (usually 0).
read :: RollingQueue a -> STM (a, Int)
read = undefined

-- | Like 'read', but do not 'retry'.  Instead, return 'Nothing' if the queue
-- is empty.
tryRead :: RollingQueue a -> STM (Maybe (a, Int))
tryRead = undefined

-- | Test if the queue is empty.
isEmpty :: RollingQueue a -> STM Bool
isEmpty = undefined

-- | Adjust the size limit.  Queue entries will be discarded if necessary to
-- satisfy the new limit.
setLimit :: Int -> RollingQueue a -> STM ()
setLimit = undefined
