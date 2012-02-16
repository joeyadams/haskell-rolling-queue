import Prelude hiding (read)

import Data.STM.RollingQueue (RollingQueue)
import qualified Data.STM.RollingQueue as RQ

import Control.Concurrent.STM
import Control.Monad

dump :: Show a => RollingQueue a -> String -> IO ()
dump rq label = do
    putStrLn $ label ++ ":"
    RQ.dump rq
    putStrLn ""

main :: IO ()
main = do
    rq <- RQ.newIO 5 :: IO (RollingQueue Int)
    dump rq "newIO 5"

    let empty = do
            atomically $ do
                Nothing <- RQ.tryRead rq
                True <- RQ.isEmpty rq
                0 <- RQ.length rq
                return ()
            dump rq "read rq, isEmpty"

        write x = do
            atomically $ do
                RQ.write rq x
                False <- RQ.isEmpty rq
                return ()
            dump rq $ "write rq " ++ show x

        read = do
            p <- atomically $ do
                False <- RQ.isEmpty rq
                RQ.read rq
            dump rq $ "read rq " ++ show p

        setLimit n = do
            atomically $ RQ.setLimit rq n
            dump rq $ "setLimit rq " ++ show n

    empty
    mapM_ write [1..10]
    replicateM_ 5 read
    empty
    mapM_ write [11..16]
    read
    4 <- atomically $ RQ.length rq

    5 <- atomically $ RQ.getLimit rq
    setLimit 3
    setLimit 10
    mapM_ write [17..30]
    replicateM_ 3 read
    setLimit (-5)
    0 <- atomically $ RQ.getLimit rq

    forM_ [31..35] $ \i -> do
        atomically $ RQ.write rq i
        True <- atomically $ RQ.isEmpty rq
        dump rq $ "write rq " ++ show i ++ " (limit is 0)"
    empty

    setLimit 1
    write 36
    read
    write 37
    read
    write 38
    write 39
    read
    empty
