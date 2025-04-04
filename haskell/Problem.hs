module Problem
(
    Problem,
    chainData,
    combination,
    newProblem,
    addLeg,
    removeLeg,
    getTotalCost

) where

import qualified Data.Set as Set
import Option
import OptionChain
import OptionLeg

-- A Problem consists of market data (OptionChain) and a combination of legs
data Problem = ProblemCon {
    chainData :: OptionChain,
    combination :: [OptionLeg]
} deriving (Show)

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
    in legCount >= 2 && cost < 0 && all (> 0) payoffs
