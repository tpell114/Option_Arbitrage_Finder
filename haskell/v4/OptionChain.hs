{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies#-}
module OptionChain
(
    OptionChain,
    emptyChain,
    addOption,
    getOptionsAtStrike,
    getAllStrikes
) where

import qualified Data.Map as Map
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Option

-- | Represents a collection of options organized by strike price
-- For each strike price, we can have a call option, a put option, or both
newtype OptionChain = ChainCon (Map.Map Double (Maybe Option, Maybe Option))
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- Create an empty option chain
emptyChain :: OptionChain
emptyChain = ChainCon Map.empty

-- | Adds an option to the chain
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

-- | Gets both the call and put options at a specific strike price
-- Returns a tuple of (Maybe Call, Maybe Put)
getOptionsAtStrike :: Double -> OptionChain -> (Maybe Option, Maybe Option)
getOptionsAtStrike strike (ChainCon chain) = Map.findWithDefault (Nothing, Nothing) strike chain

-- | Gets all strike prices available in the chain
getAllStrikes :: OptionChain -> [Double]
getAllStrikes (ChainCon chain) = Map.keys chain
