module Main where

import System.Environment (getArgs)
import System.IO
import System.Exit (exitFailure)
import Control.Exception (catch, IOException)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs, diffTimeSpec)
import Text.Printf (printf)
import Control.DeepSeq (force)
import Control.Exception (evaluate)

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

processFile :: FilePath -> IO ()
processFile filename = do
    putStrLn ("Processing file: " ++ filename)
    
    -- Catch file read errors
    chain <- loadChainFromFile filename `catch` handleIOError
    
    putStrLn "Option chain loaded:"
    printChain chain
    
    putStrLn "\nSearching for arbitrage opportunities..."

    startTime <- getTime Monotonic
    let problem = newProblem chain
    solutions <- evaluate (force (solve problem))
    endTime <- getTime Monotonic

    let diffTime = diffTimeSpec endTime startTime
    let durationMicros = fromIntegral (toNanoSecs diffTime) / 1000 :: Double
    
    printAllSolutions solutions
    
    putStrLn ("\nSearch completed in " ++ printf "%.0f" durationMicros ++ " microseconds")

-- Handle IO errors
handleIOError :: IOException -> IO a
handleIOError e = do
    putStrLn ("Error reading file: " ++ show e)
    exitFailure
