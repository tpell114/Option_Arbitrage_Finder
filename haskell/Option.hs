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

data OptionType = Call | Put
    deriving (Show, Eq, Ord)

data Option = OptionCon {
    optionType :: OptionType,
    strike :: Double,
    bid :: Double,
    ask :: Double
} deriving (Show, Eq, Ord)

-- Create a new option
newOption :: OptionType -> Double -> Double -> Double -> Option
newOption = OptionCon