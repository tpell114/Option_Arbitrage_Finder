module OptionLeg
(
    OptionLeg,
    PositionType(..),
    legOption,
    position,
    newLeg,
    getCost
) where

import Option

data PositionType = Long | Short
    deriving (Show, Eq)

data OptionLeg = LegCon {
    legOption :: Option,
    position :: PositionType
} deriving (Show)

-- Create a new option leg
newLeg :: Option -> PositionType -> OptionLeg
newLeg = LegCon

-- Calculate the cost of this leg
-- Long -> ask, Short -> -bid
getCost :: OptionLeg -> Double
getCost leg = case position leg of
    Long -> ask (legOption leg)
    Short -> negate (bid (legOption leg))