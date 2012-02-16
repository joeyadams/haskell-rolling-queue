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
{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module Data.STM.RollingQueue (
    RollingQueue,
    new,
    newIO,
    write,
    read,
    tryRead,
    isEmpty,
    length,
    setLimit,
    getLimit,

    -- * Debugging
    checkInvariants,
) where

import Prelude hiding (length, read)

import Control.Concurrent.STM hiding (check)
import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | A 'RollingQueue' is a bounded FIFO channel.  When the size limit is
-- exceeded, older entries are discarded to make way for newer entries.
--
-- Note: if the size limit is less than @1@, 'write' will have no effect, and
-- 'read' will always 'retry'.
data RollingQueue a = RQ (TVar (ReadEnd a)) (TVar (WriteEnd a))
    deriving Typeable

-- Invariants:
--
--  * writeCounter - readCounter = number of items in the queue
--
--  * writeCounter >= readCounter, since the queue count cannot be negative

instance Eq (RollingQueue a) where
    (==) (RQ r1 _) (RQ r2 _) = r1 == r2

-- | Invariants:
--
--  * readCounter >= 0, readDiscarded >= 0
data ReadEnd a =
    ReadEnd
        { readPtr       :: !(TCell a)
            -- ^ Pointer to next item in the stream
        , readCounter   :: !Int
            -- ^ Number of reads since we last synced with the writer
        , readDiscarded :: !Int
            -- ^ Number of log entries discarded since the last read
        }

-- | Invariants:
--
--  * readTVar writePtr ==> TNil
--
--  * writeCounter <= sizeLimit.  However, this will normally be false for the
--    'WriteEnd' passed to 'syncEnds'.
--
--  * writeCounter >= 0, sizeLimit >= 0
data WriteEnd a =
    WriteEnd
        { writePtr      :: !(TCell a)
            -- ^ Pointer to the hole (which is a TNil)
        , writeCounter  :: !Int
            -- ^ Write counter.  Number of items in the queue, not taking into
            -- account reads performed since the last call to 'syncEnds'.
        , sizeLimit     :: !Int
            -- ^ The size limit of the RollingQueue is stored here, in the
            --   'WriteEnd'.  This makes it convenient for 'write' to access.
        }

type TCell a = TVar (TList a)
data TList a = TNil | TCons a (TCell a)


-- | Create a new, empty 'RollingQueue', with the given size limit.
--
-- To change the size limit later, use 'setLimit'.
new :: Int -> STM (RollingQueue a)
new limit = do
    hole <- newTVar TNil
    rv <- newTVar $ ReadEnd hole 0 0
    wv <- newTVar $ WriteEnd hole 0 (max 0 limit)
    return (RQ rv wv)

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
newIO limit = do
    hole <- newTVarIO TNil
    rv <- newTVarIO $ ReadEnd hole 0 0
    wv <- newTVarIO $ WriteEnd hole 0 (max 0 limit)
    return (RQ rv wv)

-- | Write an entry to the rolling queue.  If the queue is full, discard the
-- oldest entry.
--
-- There is no @tryWrite@, because 'write' never retries.
write :: RollingQueue a -> a -> STM ()
write rq@(RQ _ wv) x = do
    w <- readTVar wv
    new_hole <- newTVar TNil
    writeTVar (writePtr w) (TCons x new_hole)
    updateWriteEnd rq $ WriteEnd new_hole (writeCounter w + 1) (sizeLimit w)

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

-- | /O(1)/ Get the number of items in the queue.
length :: RollingQueue a -> STM Int
length = undefined

-- | Adjust the size limit.  Queue entries will be discarded if necessary to
-- satisfy the new limit.
setLimit :: Int -> RollingQueue a -> STM ()
setLimit new_limit rq@(RQ _ wv) = do
    w <- readTVar wv
    updateWriteEnd rq w{sizeLimit = max 0 new_limit}

-- | Get the current size limit.
getLimit :: RollingQueue a -> STM Int
getLimit (RQ _ wv) = do
    w <- readTVar wv
    return (sizeLimit w)

------------------------------------------------------------------------
-- Internal

-- | Update the 'WriteEnd'.  If the size limit is exceeded, use 'syncEnds'.
updateWriteEnd :: RollingQueue a -> WriteEnd a -> STM ()
updateWriteEnd (RQ rv wv) w
    | writeCounter w <= sizeLimit w
    = writeTVar wv w
    | otherwise = do
        r <- readTVar rv
        (r', w') <- syncEnds r w
        writeTVar rv r'
        writeTVar wv w'

-- | Sync the reader and writer.  This sets the ReadEnd's counter to 0, and
-- discards old log entries to satisfy the size limit (if necessary).
syncEnds :: ReadEnd a -> WriteEnd a -> STM (ReadEnd a, WriteEnd a)
syncEnds r w = do
    let count = writeCounter w - readCounter r
        limit = sizeLimit w
    if count > limit
        then do
            let drop_count = count - limit
            rp' <- dropItems drop_count (readPtr r)
            return ( ReadEnd rp' 0 (readDiscarded r + drop_count)
                   , WriteEnd (writePtr w) limit limit
                   )
        else
            return ( ReadEnd (readPtr r) 0 (readDiscarded r)
                   , WriteEnd (writePtr w) count limit
                   )

-- | TCell variant of 'drop'.  This does not modify the cells themselves, it
-- just returns the new pointer.
dropItems :: Int -> TCell a -> STM (TCell a)
dropItems n cell
    | n <= 0    = return cell
    | otherwise = do
        xs <- readTVar cell
        case xs of
            TNil          -> return cell
            TCons _ cell' -> dropItems (n-1) cell'

------------------------------------------------------------------------
-- Debugging

data CheckException = CheckNotTrue String | CheckMsg String
    deriving Typeable

instance Show CheckException where
    show (CheckNotTrue expr) =
        "Data.STM.RollingQueue checkInvariants: " ++ expr ++ " does not hold"
    show (CheckMsg msg) =
        "Data.STM.RollingQueue checkInvariants: " ++ msg

instance Exception CheckException

checkInvariants :: RollingQueue a -> STM ()
checkInvariants (RQ rv wv) = do
    r <- readTVar rv
    w <- readTVar wv

    check (readCounter   r >= 0) "readCounter >= 0"
    check (readDiscarded r >= 0) "readDiscarded >= 0"

    check (writeCounter w >= 0) "writeCounter >= 0"
    check (sizeLimit    w >= 0) "sizeLimit >= 0"
    check (writeCounter w <= sizeLimit w) "writeCounter <= sizeLimit"
    hole <- readTVar (writePtr w)
    case hole of
        TNil      -> return ()
        TCons _ _ -> throwSTM (CheckMsg "writePtr does not point to a TNil")

    check (writeCounter w >= readCounter r) "writeCounter >= readCounter"
    len <- traverseLength (readPtr r)
    check (writeCounter w - readCounter r == len) "writeCounter - readCounter == length"

    where
        check b expr | b         = return ()
                     | otherwise = throwSTM (CheckNotTrue expr)

traverseLength :: TCell a -> STM Int
traverseLength = loop 0
    where
        loop !n cell = do
            xs <- readTVar cell
            case xs of
                TNil          -> return n
                TCons _ cell' -> loop (n+1) cell'
