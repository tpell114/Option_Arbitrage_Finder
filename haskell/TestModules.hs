module TestModules where

import Option
import OptionChain
import OptionLeg
import Problem
import qualified Data.Set as Set

-- Create test options with box spread arbitrage
testOptions :: [Option]
testOptions = [
    newOption Call 100.0 9.8 10.0,   -- Call bid=9.8, ask=10.0
    newOption Put 100.0 4.8 5.0,     -- Put bid=4.8, ask=5.0
    newOption Call 110.0 2.8 3.0,    -- Call bid=2.8, ask=3.0
    newOption Put 110.0 12.8 13.0    -- Put bid=12.8, ask=13.0
    ]

buildTestChain :: OptionChain
buildTestChain = foldr addOption emptyChain testOptions

testBoxSpread :: IO ()
testBoxSpread = do
    let chain = buildTestChain
    let problem = newProblem chain
    let solutions = solve problem
    
    putStrLn ("Found " ++ show (length solutions) ++ " solutions")
    
    case solutions of
        [] -> putStrLn "No arbitrage opportunities found."
        _ -> do
            putStrLn "Arbitrage opportunities found!"
            mapM_ printSolution (zip [1..] solutions)

-- Print a single solution
printSolution :: (Int, Problem) -> IO ()
printSolution (solutionNum, solution) = do
    putStrLn ("\n--- Solution " ++ show solutionNum ++ " ---")
    putStrLn ("Total cost: " ++ show (getTotalCost solution))
    putStrLn "Combination:"
    printCombination (combination solution)
    putStrLn "Payoffs at critical prices:"
    printPayoffs solution

-- Print combination legs
printCombination :: [OptionLeg] -> IO ()
printCombination legs = mapM_ printLeg (zip [1..] legs)
    where
        printLeg (i, leg) = do
            let opt = legOption leg
            putStrLn (show i ++ ". " ++ show (position leg) ++ " " ++ show (optionType opt) 
                        ++ " at strike " ++ show (strike opt) ++ " (cost: " ++ show (getCost leg) ++ ")")

-- Print payoffs at critical prices
printPayoffs :: Problem -> IO ()
printPayoffs prob = 
    mapM_ (\price -> putStrLn ("At price " ++ show price ++ 
                              ": " ++ show (calculatePayoffAt price prob)))
          (Set.toList (getCriticalPrices prob))

main :: IO ()
main = testBoxSpread