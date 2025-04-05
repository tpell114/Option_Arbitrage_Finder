{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
module Problem
(
    Problem,
    chainData,
    combination,
    newProblem,
    addLeg,
    removeLeg,
    getTotalCost,
    getCriticalPrices,
    calculatePayoffAt,
    getPossibleMoves,
    isSolved,
    solve

) where

import qualified Data.Set as Set
import Data.List (sort)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Option
import OptionChain
import OptionLeg

-- A Problem consists of market data (OptionChain) and a combination of legs
data Problem = ProblemCon {
    chainData :: OptionChain,
    combination :: [OptionLeg]
} deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- Create a new problem with empty combination
newProblem :: OptionChain -> Problem
newProblem chain = ProblemCon chain []

-- Add leg to the combination at the head
addLeg :: OptionLeg -> Problem -> Problem
addLeg leg prob = prob {combination = leg : combination prob}

-- Remove head leg from the combination
removeLeg :: Problem -> Problem
removeLeg prob = case combination prob of
    [] -> prob
    (_:xs) -> prob {combination = xs}

-- Calculate the total cost of the combination
-- Returns cost (debit/credit)
getTotalCost :: Problem -> Double
getTotalCost prob = sum (map getCost (combination prob))

-- Get all critical prices, each unique strike and 0 and 10000
getCriticalPrices :: Problem -> Set.Set Double
getCriticalPrices prob = Set.fromList ([0.0, 10000.0] ++ [strike (legOption leg) | leg <- combination prob])

-- Calculate payoff at a specific price
-- Price -> Problem -> Payoff
calculatePayoffAt :: Double -> Problem -> Double
calculatePayoffAt price prob =
    let initialPayoff = negate (getTotalCost prob)  -- start with -cost
    in initialPayoff + sum [legPayoff leg | leg <- combination prob]
    where
        legPayoff leg =
            let opt = legOption leg
                intrinsicValue = case optionType opt of
                    Call -> max 0.0 (price - strike opt)
                    Put -> max 0.0 (strike opt - price)
            in case position leg of
                Long -> intrinsicValue
                Short -> negate intrinsicValue

-- Get all possible legs that can be added to the combination
-- Problem -> Posible moves
getPossibleMoves :: Problem -> [OptionLeg]
getPossibleMoves prob =
    if length (combination prob) >= 4
    then []  -- Maximum 4 legs
    else concat [getLegsForStrike strikePrice | strikePrice <- getAllStrikes (chainData prob)]
    where
        getLegsForStrike strikePrice =
            let (maybeCall, maybePut) = getOptionsAtStrike strikePrice (chainData prob)
                usedOptions = map legOption (combination prob)

                callLegs = case maybeCall of
                    Nothing -> []
                    Just opt -> if opt `elem` usedOptions
                               then []
                               else [newLeg opt Long, newLeg opt Short]

                putLegs = case maybePut of
                    Nothing -> []
                    Just opt -> if opt `elem` usedOptions
                              then []
                              else [newLeg opt Long, newLeg opt Short]
            in callLegs ++ putLegs

-- Check if the combination provides an arbitrage opportunity
isSolved :: Problem -> Bool
isSolved prob =
    let legCount = length (combination prob)
        cost = getTotalCost prob
        criticalPrices = Set.toList (getCriticalPrices prob)
        payoffs = map (`calculatePayoffAt` prob) criticalPrices
    in legCount >= 2 && all (>= 0.01) payoffs



-- Solve the problem recursively, returning all possible solutions
-- Problem -> [Solution]
solve :: Problem -> [Problem]
solve prob 
    | isSolved prob = [prob]  -- There's one solution if already solved
    | otherwise = uniqueSolutions 
                  [ solution 
                  | length (combination prob) < 4,  -- Check maximum leg count
                    move <- getPossibleMoves prob,  -- Generate possible moves
                    let prob' = addLeg move prob,   -- Apply move to problem
                    solution <- solve prob'         -- Recursively solve
                  ]
    where
        -- Helper to remove duplicate solutions that just have legs in different orders
        uniqueSolutions :: [Problem] -> [Problem]
        uniqueSolutions [] = []
        uniqueSolutions (x:xs) = x : uniqueSolutions (filter (not . hasSameLegs x) xs)
        
        -- Check if two problems have the same set of legs (ignoring order)
        hasSameLegs :: Problem -> Problem -> Bool
        hasSameLegs p1 p2 = sort (combination p1) == sort (combination p2)

