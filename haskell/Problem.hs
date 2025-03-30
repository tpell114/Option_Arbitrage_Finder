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
getTotalCost :: Problem -> Double
getTotalCost prob = sum (map getCost (combination prob))

