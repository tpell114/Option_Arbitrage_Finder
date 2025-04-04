module IO
(
    loadChainFromFile,
    printChain,
    printSolution,
    printAllSolutions
) where

import System.IO
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Text.Printf (printf)
import Option
import OptionChain
import OptionLeg
import Problem

-- Load option chain from a CSV file
-- Format: type,strike,bid,ask
-- Example: call,100,9.8,10.0
loadChainFromFile :: FilePath -> IO OptionChain
loadChainFromFile filepath = do
    content <- readFile filepath
    let rows = drop 1 (lines content)  -- Skip header row
    let options = map parseOptionRow rows
    return (foldr addOption emptyChain options)

-- Parse a single row into an Option
parseOptionRow :: String -> Option
parseOptionRow row =
    case splitOn "," row of
        [typeStr, strikeStr, bidStr, askStr] ->
            let optType = if typeStr == "call" then Call else Put
                strikeVal = read strikeStr :: Double
                bidVal = read bidStr :: Double
                askVal = read askStr :: Double
            in newOption optType strikeVal bidVal askVal
        _ -> error ("Invalid row format: " ++ row)

-- Print the option chain in a formatted manner
printChain :: OptionChain -> IO ()
printChain chain = do
    putStrLn "Option Chain:"
    putStrLn "--------------------------------------------------"
    putStrLn "Call Bid | Call Ask | Strike | Put Bid  | Put Ask"
    putStrLn "--------------------------------------------------"
    
    mapM_ printStrikePair (getAllStrikes chain)
    putStrLn "--------------------------------------------------"
    where
        printStrikePair strike = do
            let (maybeCall, maybePut) = getOptionsAtStrike strike chain
            let callBid = maybe "N/A" (formatPrice . bid) maybeCall
            let callAsk = maybe "N/A" (formatPrice . ask) maybeCall
            let putBid = maybe "N/A" (formatPrice . bid) maybePut
            let putAsk = maybe "N/A" (formatPrice . ask) maybePut
            
            putStrLn (padRight 8 callBid ++ " | " ++
                      padRight 8 callAsk ++ " | " ++
                      padRight 6 (formatPrice strike) ++ " | " ++
                      padRight 8 putBid ++ " | " ++
                      padRight 8 putAsk)
                       
        padRight n str = take n (str ++ repeat ' ')
        formatPrice = printf "%.2f"

-- Print a solution (arbitrage opportunity)
printSolution :: Problem -> IO ()
printSolution solution = do
    putStrLn "\nArbitrage opportunity found!"
    putStrLn "--------------------------------"
    putStrLn ("Net Credit: $" ++ formatPrice (negate (getTotalCost solution)))
    putStrLn "Legs:"
    putStrLn ""
    
    let legs = combination solution
    mapM_ (\(i, leg) -> do
        let opt = legOption leg
        putStrLn (show (i+1) ++ ". " ++
                  (if position leg == Long then "Long " else "Short ") ++
                  (if optionType opt == Call then "Call" else "Put") ++
                  " Strike: $" ++ formatPrice (strike opt) ++
                  " Price: $" ++ formatPrice (abs (getCost leg)))
        ) (zip [0..] legs)
    
    putStrLn "\nPayoff at critical prices:"
    let criticalPrices = Set.toList (getCriticalPrices solution)
    mapM_ (\price -> do
        let payoff = calculatePayoffAt price solution
        putStrLn ("Price: $" ++ formatPrice price ++ " -> Profit: $" ++ formatPrice payoff)
        ) criticalPrices
    putStrLn "--------------------------------"
    where
        formatPrice = printf "%.2f"

-- Print all solutions
printAllSolutions :: [Problem] -> IO ()
printAllSolutions [] = putStrLn "No arbitrage opportunities found."
printAllSolutions solutions = do
    putStrLn ("Found " ++ show (length solutions) ++ " arbitrage opportunities.")
    mapM_ printSolution solutions



    