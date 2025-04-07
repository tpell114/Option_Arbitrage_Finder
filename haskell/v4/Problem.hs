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
    solve,
    solveParallel

) where

import qualified Data.Set as Set
import Data.List (sort)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData, force)
import Control.Parallel.Strategies (using, parList, rdeepseq, Strategy, rpar)
import Control.Monad (when)
import Option
import OptionChain
import OptionLeg

-- | Represents the state of an arbitrage search
-- Contains the market data and the current combination of option legs
data Problem = ProblemCon {
    chainData :: OptionChain,   -- ^ The option chain containing market data
    combination :: [OptionLeg]  -- ^ The current combination of option legs
} deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | Creates a new problem with the given option chain and an empty combination
newProblem :: OptionChain -> Problem
newProblem chain = ProblemCon chain []

-- | Adds a leg to the combination at the head
addLeg :: OptionLeg -> Problem -> Problem
addLeg leg prob = prob {combination = leg : combination prob}

-- | Remove head leg from the combination
removeLeg :: Problem -> Problem
removeLeg prob = case combination prob of
    [] -> prob
    (_:xs) -> prob {combination = xs}

-- | Calculates the total cost of the combination
-- Returns cost (debit if positive, credit if negative)
getTotalCost :: Problem -> Double
getTotalCost prob = sum (map getCost (combination prob))

-- | Gets all critical prices, which are the unique strikes plus 0 and 10000
-- Critical prices are points where the payoff function changes slope
getCriticalPrices :: Problem -> Set.Set Double
getCriticalPrices prob = Set.fromList ([0.0, 10000.0] ++ [strike (legOption leg) | leg <- combination prob])

-- | Calculates the payoff at a specific price
-- Takes into account the initial cost and the intrinsic value of all legs
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

-- | Get all possible legs that can be added to the combination
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
                    Just opt ->
                        if opt `elem` usedOptions
                        then []
                        -- Skip adding short position for options with zero bid
                        else if bid opt <= 0.00
                             then [newLeg opt Long]
                             else [newLeg opt Long, newLeg opt Short]

                putLegs = case maybePut of
                    Nothing -> []
                    Just opt ->
                        if opt `elem` usedOptions
                        then []
                        -- Skip adding short position for options with zero bid
                        else if bid opt <= 0.00
                             then [newLeg opt Long]
                             else [newLeg opt Long, newLeg opt Short]
            in callLegs ++ putLegs

-- | Checks if the combination provides an arbitrage opportunity
-- An arbitrage exists if:
-- 1. The combination has at least 2 legs
-- 2. The payoff is positive at all critical prices
isSolved :: Problem -> Bool
isSolved prob =
    let legCount = length (combination prob)
        cost = getTotalCost prob
        criticalPrices = Set.toList (getCriticalPrices prob)
        payoffs = map (`calculatePayoffAt` prob) criticalPrices
    in legCount >= 2 && all (>= 0.01) payoffs


-- | Solves the problem recursively, returning all possible solutions
-- Uses a depth-first search with backtracking to find all valid arbitrage combinations
solve :: Problem -> [Problem]
solve prob = removeDuplicates (findAllSolutions prob)
    where
    findAllSolutions p =
        let currentSolutions = ([p | isSolved p])
            furtherSolutions =
                if length (combination p) >= 4
                then []  -- Maximum 4 legs reached, don't explore further
                else [ solution
                     | move <- getPossibleMoves p,  -- Generate possible moves
                       let p' = addLeg move p,      -- Apply move to problem
                       solution <- findAllSolutions p'  -- Recursively solve
                     ]
        in currentSolutions ++ furtherSolutions

    -- Helper to remove duplicate solutions that just have legs in different orders
    removeDuplicates :: [Problem] -> [Problem]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (not . hasSameLegs x) xs)

    -- Check if two problems have the same set of legs (ignoring order)
    hasSameLegs :: Problem -> Problem -> Bool
    hasSameLegs p1 p2 = sort (combination p1) == sort (combination p2)
    
-- | Parallel version of the solve function
-- Uses parallel strategies to speed up the search
solveParallel :: Problem -> [Problem]
solveParallel prob = removeDuplicates (findAllSolutionsParallel prob)
    where
    -- Recursive function to find all solutions with parallelism
    findAllSolutionsParallel p =
        let currentSolutions = ([p | isSolved p])
            moves = getPossibleMoves p
        in 
            if length (combination p) >= 4 
            then currentSolutions  -- Maximum 4 legs reached, don't explore further
            else currentSolutions ++ 
                 if length moves <= 1
                 then -- For small number of moves, don't parallelize
                      concat [findAllSolutionsParallel (addLeg move p) | move <- moves]
                 else -- Parallelize across the different next moves
                      concat (map (\move -> findAllSolutionsParallel (addLeg move p)) moves 
                              `using` parList rdeepseq)

    -- Helper to remove duplicate solutions that just have legs in different orders
    removeDuplicates :: [Problem] -> [Problem]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (not . hasSameLegs x) xs)

    -- Check if two problems have the same set of legs (ignoring order)
    hasSameLegs :: Problem -> Problem -> Bool
    hasSameLegs p1 p2 = sort (combination p1) == sort (combination p2)