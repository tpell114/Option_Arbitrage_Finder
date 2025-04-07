{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies#-}
module Option
(
    Option,
    OptionType(..),
    newOption,
    optionType,
    strike,
    bid,
    ask
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Represents the type of an option: either a Call or a Put option
data OptionType = Call | Put
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Represents an option with its key properties
data Option = OptionCon {
    optionType :: OptionType, -- ^ The type of the option (Call or Put)
    strike :: Double,         -- ^ The strike price of the option
    bid :: Double,            -- ^ The bid price of the option
    ask :: Double             -- ^ The ask price of the option
} deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Creates a new option with the specified parameters
newOption :: OptionType -> Double -> Double -> Double -> Option
newOption = OptionCon
