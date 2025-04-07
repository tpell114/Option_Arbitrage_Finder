module Main where

import System.Environment (getArgs)
import System.IO
import System.Exit (exitFailure)
import Control.Exception (catch, IOException, evaluate)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs, diffTimeSpec)
import Text.Printf (printf)
import Control.DeepSeq (force)
import Control.Monad (when)
import Control.Parallel.Strategies (using, parList, rdeepseq)
import GHC.Conc (getNumCapabilities, setNumCapabilities)

import Option
import OptionChain
import OptionLeg
import Problem
import IO

main :: IO ()
main = do
    args <- getArgs

    case args of
        [filename] -> processFile filename
        _ -> do
            putStrLn "Usage: options-arbitrage <input-file>"
            exitFailure

-- | Processes an options data file to find arbitrage opportunities
-- Loads the option chain, prints it, and then searches for arbitrage opportunities
-- Also measures and reports the performance of the search
processFile :: FilePath -> IO ()
processFile filename = do
    putStrLn ("Processing file: " ++ filename)
    
    numCores <- getNumCapabilities
    putStrLn $ "Using parallel processing with " ++ show numCores ++ " threads"

    chain <- loadChainFromFile filename `catch` handleIOError

    putStrLn "Option chain loaded:"
    printChain chain

    putStrLn "\nSearching for arbitrage opportunities..."

    let problem = newProblem chain
    let loops = 5
    totalDuration <- runSearchLoop problem loops 1 0
    
    putStrLn "--------------------------------"
    putStrLn ("\nAverage search time for " ++ show loops ++ " iterations: "
              ++ printf "%.0f" (totalDuration / fromIntegral loops) ++ " microseconds.")

    where
        runSearchLoop _ loops currentLoop totalTime | currentLoop > loops = return totalTime
        runSearchLoop prob loops currentLoop totalTime = do
            startTime <- getTime Monotonic
            solutions <- evaluate (force (solveParallel prob))
            endTime <- getTime Monotonic

            let diffTime = diffTimeSpec endTime startTime
            let durationMicros = fromIntegral (toNanoSecs diffTime) / 1000 :: Double

            -- Only print solutions on the first loop
            when (currentLoop == 1) (printAllSolutions solutions)

            putStrLn ("\nSearch completed in " ++ printf "%.0f" durationMicros ++ " microseconds")

            runSearchLoop prob loops (currentLoop + 1) (totalTime + durationMicros)

-- | Handles IO errors when reading files
-- Prints an error message and exits with failure
handleIOError :: IOException -> IO a
handleIOError e = do
    putStrLn ("Error reading file: " ++ show e)
    exitFailure
