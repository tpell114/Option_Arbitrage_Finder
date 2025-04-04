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

data PositionType = Long | Short
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

data OptionLeg = LegCon {
    legOption :: Option,
    position :: PositionType
} deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    
-- Create a new option leg
newLeg :: Option -> PositionType -> OptionLeg
newLeg = LegCon

-- Calculate the cost of this leg
-- Long -> ask, Short -> -bid
getCost :: OptionLeg -> Double
getCost leg = case position leg of
    Long -> ask (legOption leg)
    Short -> negate (bid (legOption leg))