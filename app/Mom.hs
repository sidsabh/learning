module Mom where
-- import Debug.Trace ( traceShowId ) -- super useful like id fcn
import Data.List(sort)
import Control.Exception ( assert )
import Text.Read (readMaybe)
import System.Random (Random(randomRs), getStdGen)


minGen, maxGen :: Int
minGen = -100; maxGen = 100

chunked :: Int -> [a] -> [[a]]
chunked _ [] = [] --- without this, no warning but get infinite loops
chunked n xs = take n xs : chunked n (drop n xs)

-- Guaranteed to chunk in 5 so median is !! 2
medianOfMedians :: [Int] -> Float
medianOfMedians xs = 
    if n < 5
        then
            median xs False
        else 
            mom
    where 
        n = length xs
        chunks = assert (n > 0) chunked 5 xs
        full_chunks = filter (\x -> length x == 5) chunks
        sorted_chunks = [sort c | c <- full_chunks]
        medians = [l!!2 | l <- sorted_chunks]
        mom = median medians True


select :: [Int] -> Int -> Int
select xs k =
    let pivot = medianOfMedians xs
        lows = [a | a <- xs, fromIntegral a < pivot]
        highs = [a | a <- xs, fromIntegral a > pivot]
        pivots = [a | a <- xs, fromIntegral a == pivot]
        idx | k < length lows = select lows k
            | k < (length lows + length pivots) = head pivots
            | otherwise = select highs (k - length lows - length pivots)
    in idx


median :: [Int] -> Bool -> Float
median xs linear = if even n
    then (lMed + rMed) / 2
    else lMed
  where
    n = length xs
    mIdx = n `div` 2
    middles = if linear
        then [fromIntegral $ select xs i | i <- [mIdx, mIdx-1]]
        else let sorted = sort xs in map (fromIntegral . (sorted !!)) [mIdx, mIdx-1]
    lMed = head middles
    rMed = last middles

getLen :: IO Int
getLen = do
    input <- getLine
    case readMaybe input of
        Just len | len > 0 -> return len
        _ -> do
            putStrLn "Invalid number! Please enter a positive integer."
            getLen

main :: IO ()
main = do
    putStrLn "How long would you like your list to be?"
    len <- getLen
    gen <- getStdGen
    let randomList = take len $ randomRs (minGen, maxGen) gen
        linearMedian = median randomList True
    print randomList
    print $ assert (linearMedian == median randomList False) linearMedian