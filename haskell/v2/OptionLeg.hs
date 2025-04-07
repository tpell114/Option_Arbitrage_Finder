{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
module OptionLeg
(
    OptionLeg,
    PositionType(..),
    legOption,
    position,
    newLeg,
    getCost
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Option

-- | Represents the direction of an option position: either Long (buying) or Short (selling)
data PositionType = Long | Short
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Represents a single leg in an option combination strategy
data OptionLeg = LegCon {
    legOption :: Option,        -- ^ The option contract
    position :: PositionType    -- ^ Whether we are long (buying) or short (selling) the option
} deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    
-- | Creates a new option leg with the specified option and position type
newLeg :: Option -> PositionType -> OptionLeg
newLeg = LegCon

-- | Calculates the cost of this leg
-- For a Long position, we pay the ask price
-- For a Short position, we receive the bid price (negative cost)
getCost :: OptionLeg -> Double
getCost leg = case position leg of
    Long -> ask (legOption leg)
    Short -> negate (bid (legOption leg))
    