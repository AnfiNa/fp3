module Types (
    Point (Point, px, py),
)
where

data Point = Point
    { px :: Double
    , py :: Double
    }
    deriving (Show, Eq)
