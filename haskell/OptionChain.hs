module OptionChain
(
    OptionChain,
    emptyChain,
    addOption,
    getOptionsAtStrike,
    getAllStrikes
) where

import qualified Data.Map as Map
import Option

-- Map(Strike -> (call, put))
newtype OptionChain = ChainCon (Map.Map Double (Maybe Option, Maybe Option))
    deriving (Show)

-- Create an empty option chain
emptyChain :: OptionChain
emptyChain = ChainCon Map.empty

-- Add an option to the chain
addOption :: Option -> OptionChain -> OptionChain
addOption opt (ChainCon chain) = ChainCon (Map.alter updatePair (strike opt) chain)
    where
        updatePair Nothing = Just (maybeCall, maybePut)
        updatePair (Just (existingCall, existingPut)) = Just (newCall, newPut)
            where
                newCall = if isCall then Just opt else existingCall
                newPut = if not isCall then Just opt else existingPut
    
        isCall = optionType opt == Call
        maybeCall = if isCall then Just opt else Nothing
        maybePut = if not isCall then Just opt else Nothing

-- Get both options at a specific strike price
-- strike -> chain -> (maybe call, maybe put)
getOptionsAtStrike :: Double -> OptionChain -> (Maybe Option, Maybe Option)
getOptionsAtStrike strike (ChainCon chain) = Map.findWithDefault (Nothing, Nothing) strike chain

-- Get all strike prices in the chain
-- chain -> [strikes]
getAllStrikes :: OptionChain -> [Double]
getAllStrikes (ChainCon chain) = Map.keys chain