module Palindrome where
  
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Char as Char
import System.Random ( randomRIO )
import Control.Monad (replicateM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Parallel.Strategies (using, parList, rseq)

-- Define the palindrome check function using Text
isPalindrome :: Text -> Bool
isPalindrome s = isPal cleanS
  where
    cleanS = T.filter Char.isAlphaNum $ T.toLower s
    isPal t = t == T.reverse t

-- Generate a random alphanumeric string
generateRandomString :: Int -> IO Text
generateRandomString len = T.pack <$> replicateM len randomChar
  where
    randomChar = randomRIO (' ', 'z')  -- covers the range of characters used in the C++ example

-- Create a palindrome by mirroring a random string
createPalindrome :: Int -> IO Text
createPalindrome len = do
    half <- generateRandomString len
    return $ T.concat [half, T.reverse half]

-- Benchmarking utility 
benchmark :: IO ()
benchmark = do
    start <- getCurrentTime
    palindromes <- replicateM 1000 (createPalindrome 10000)
    randomTexts <- replicateM 1000 (generateRandomString 10000)
    let testStrings = palindromes ++ randomTexts
    let results = map isPalindrome testStrings `using` parList rseq
    print $ "Number of palindromes: " ++ show (length $ filter id results)
    end <- getCurrentTime
    print $ "Time per operation: " ++ show ((diffUTCTime end start) / fromIntegral (length testStrings)) ++ " seconds"

main :: IO ()
main = benchmark
