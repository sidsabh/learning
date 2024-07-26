module SantaClausSTM where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, check, newTVar, orElse, readTVar, writeTVar)
import Control.Monad (forever, join)
import System.Random (Random (randomR), getStdRandom)

--- Gate ---
data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do tv <- newTVar 0; return (MkGate n tv)

passGate :: Gate -> IO ()
passGate (MkGate _ tv) =
  atomically
    ( do
        n_left <- readTVar tv
        check (n_left > 0)
        writeTVar tv (n_left - 1)
    )

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
  atomically (writeTVar tv n)
  atomically
    ( do
        n_left <- readTVar tv
        check (n_left == 0)
    )

--- Group ---
data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n =
  atomically
    ( do
        g1 <- newGate n
        g2 <- newGate n
        tv <- newTVar (n, g1, g2)
        return (MkGroup n tv)
    )

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup _ tv) =
  atomically
    ( do
        (n_left, g1, g2) <- readTVar tv
        check (n_left > 0)
        writeTVar tv (n_left - 1, g1, g2)
        return (g1, g2)
    )

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
  (n_left, g1, g2) <- readTVar tv
  check (n_left == 0)
  new_g1 <- newGate n
  new_g2 <- newGate n
  writeTVar tv (n, new_g1, new_g2)
  return (g1, g2)

-- Utilities ---
helper1 :: Group -> IO () -> IO ()
helper1 group do_task = do
  (in_gate, out_gate) <- joinGroup group
  passGate in_gate
  do_task
  passGate out_gate

randomDelay :: IO ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelay = do
  waitTime <- getStdRandom (randomR (1, 1000000))
  threadDelay waitTime

--- Santa ---
choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = do join (atomically (foldr1 orElse actions))
  where
    actions :: [STM (IO ())]
    actions = [do rhs <$> guard | (guard, rhs) <- choices]

santa :: Group -> Group -> IO ()
santa elf_gp rein_gp = do
  putStr "----------\n"
  choose
    [ (awaitGroup rein_gp, run "deliver toys"),
      (awaitGroup elf_gp, run "meet in my study")
    ]
  where
    run :: String -> (Gate, Gate) -> IO ()
    run task (in_gate, out_gate) = do
      putStrLn ("Ho! Ho! Ho! let’s " ++ task)
      operateGate in_gate
      operateGate out_gate

--- Elves ---
meetInStudy :: Int -> IO ()
meetInStudy idx = putStr ("Elf " ++ show idx ++ " meeting in the study\n")

elf1 :: Group -> Int -> IO ()
elf1 gp idx = helper1 gp (meetInStudy idx)

elf :: Group -> Int -> IO ThreadId
elf gp idx = forkIO (forever (do elf1 gp idx; randomDelay))

--- Reindeeer ---
deliverToys :: Int -> IO ()
deliverToys idx = putStr ("Reindeer " ++ show idx ++ " delivering toys\n")

reindeer1 :: Group -> Int -> IO ()
reindeer1 gp idx = helper1 gp (deliverToys idx)

reindeer :: Group -> Int -> IO ThreadId
reindeer gp idx = forkIO (forever (do reindeer1 gp idx; randomDelay))

--- Run --
main :: IO ()
main = do
  elf_group <- newGroup 3
  sequence_ [elf elf_group n | n <- [1 .. 10]]
  rein_group <- newGroup 9
  sequence_ [reindeer rein_group n | n <- [1 .. 9]]
  forever (santa elf_group rein_group)