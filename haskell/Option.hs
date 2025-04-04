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

data OptionType = Call | Put
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

data Option = OptionCon {
    optionType :: OptionType,
    strike :: Double,
    bid :: Double,
    ask :: Double
} deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- Create a new option
newOption :: OptionType -> Double -> Double -> Double -> Option
newOption = OptionCon