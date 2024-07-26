module BankAccountSTM where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (STM, TVar, atomically, check, orElse, newTVar, readTVar, writeTVar, readTVarIO)

import SantaClausSTM

--- bank account
type Account = TVar Int

makeAccount :: Int -> STM Account
makeAccount = newTVar

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
  bal <- readTVar acc
  writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM ()
deposit acc amount = withdraw acc (-amount)

transfer :: Account -> Account -> Int -> IO ()
-- Transfer ’amount’ from account ’from’ to account ’to’
transfer from to amount =
  atomically
    ( do
        deposit to amount
        withdraw from amount
    )

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do
  bal <- readTVar acc
  check (amount <= 0 || amount <= bal)
  writeTVar acc (bal - amount)

limitedWithdraw2 :: Account -> Account -> Int -> STM ()
-- (limitedWithdraw2 acc1 acc2 amt) withdraws amt from acc1,
-- if acc1 has enough money, otherwise from acc2.
-- If neither has enough, it retries.
limitedWithdraw2 acc1 acc2 amt =
  orElse
    (limitedWithdraw acc1 amt)
    (limitedWithdraw acc2 amt)

printBalance :: Account -> IO ()
printBalance acc = do
  balance <- readTVarIO acc
  putStrLn $ "Current balance: " ++ show balance

printBalances :: [Account] -> IO ()
printBalances = mapM_ printBalance

main :: IO ()
main = do
  acc1 <- atomically $ makeAccount 100
  acc2 <- atomically $ makeAccount 0
  acc3 <- atomically $ makeAccount 0
  
  putStrLn "Initial balances:"
  printBalances [acc1, acc2, acc3]
  
  putStrLn "\nStarting concurrent transactions..."

  done1 <- newEmptyMVar
  done2 <- newEmptyMVar

  -- Concurrent transfer from acc1 to acc2
  _ <- forkIO $ do
    randomDelay
    transfer acc1 acc3 30
    putStrLn "Transfer of 30 from Account 1 to Account 2 done."
    putMVar done1 ()

  -- Concurrent limited withdrawal from either account
  _ <- forkIO $ do
    randomDelay
    atomically $ limitedWithdraw2 acc3 acc1 25
    putStrLn "Limited withdrawal of 25 from either Account 1 or Account 2 done."
    putMVar done2 ()
  
  -- Wait for both concurrent operations to complete
  takeMVar done1
  takeMVar done2

  putStrLn "\nBalances after concurrent transactions:"
  printBalances [acc1, acc2, acc3]

  putStrLn "\nDone!"
