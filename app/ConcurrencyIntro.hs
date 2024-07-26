module ConcurrencyIntro where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (MVar)
import Control.Monad (unless, replicateM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Constants
start :: Int
start = 100

-- Transfer amount from one IORef to another
transfer :: IORef Int -> IORef Int -> Int -> IO ()
transfer fromRef toRef amount = do
  fromVal <- readIORef fromRef
  toVal <- readIORef toRef
  writeIORef fromRef (fromVal - amount)
  threadDelay 100000 -- Expose inconsistent state
  writeIORef toRef (toVal + amount)

-- Print the value of an IORef
printRef :: IORef Int -> IO ()
printRef ref = do
  val <- readIORef ref
  putStrLn $ "Current value: " ++ show val

-- Print the values of multiple IORefs with labels
printValues :: [(String, IORef Int)] -> IO ()
printValues =
  mapM_
    ( \(label, ref) ->
        do
          putStr (label ++ ": ")
          printRef ref
    )

-- Concurrent transfer operation
concurrentTransfer :: IORef Int -> IORef Int -> Int -> MVar () -> IO ()
concurrentTransfer fromRef toRef amount done = do
  transfer fromRef toRef amount
  putStrLn $ "Transferred " ++ show amount ++ " from one account to another."
  putMVar done ()

-- Check if the sum of the values of the IORefs matches the start value
checkSum :: [IORef Int] -> IO Bool
checkSum refs = do
  values <- mapM readIORef refs
  let total = sum values
  return (total == start)

-- Function to continuously print account values
continuousPrint :: [(String, IORef Int)] -> MVar () -> IO ()
continuousPrint refs done = do
  let loop = do
        consistent <- checkSum [i | (_, i) <- refs]
        unless consistent $ do 
          putStrLn "Inconsistent state detected!"
          printValues refs
          putStrLn ""
        threadDelay 100000 -- Print delay
        loop
  _ <- forkIO loop
  takeMVar done

-- Function to create multiple accounts
createAccounts :: [Int] -> IO [IORef Int]
createAccounts = mapM newIORef

-- Function to create multiple MVars
createMVars :: Int -> IO [MVar ()]
createMVars n = replicateM n newEmptyMVar

main :: IO ()
main = do
  -- Create IORefs to represent accounts
  [acc1, acc2, acc3] <- createAccounts [start, 0, 0]

  putStrLn "Initial values:"
  printValues [("Account 1", acc1), ("Account 2", acc2), ("Account 3", acc3)]

  putStrLn "\nStarting concurrent operations..."

  -- Create MVars for synchronization
  [done1, done2, done3] <- createMVars 3
  continuousDone <- newEmptyMVar

  -- Start continuous printing in a separate thread
  _ <- forkIO $ continuousPrint [("Account 1", acc1), ("Account 2", acc2), ("Account 3", acc3)] continuousDone

  -- Concurrent transfers
  _ <- forkIO $ concurrentTransfer acc1 acc2 50 done1
  _ <- forkIO $ concurrentTransfer acc1 acc2 50 done2
  _ <- forkIO $ concurrentTransfer acc2 acc3 50 done3

  -- Wait for all concurrent operations to complete
  takeMVar done1
  takeMVar done2
  takeMVar done3
  putMVar continuousDone ()

  putStrLn "Values after concurrent operations:"
  printValues [("Account 1", acc1), ("Account 2", acc2), ("Account 3", acc3)]

  putStrLn "\nDone!"
