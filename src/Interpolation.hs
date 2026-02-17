module Interpolation
  ( Algorithm (..),
    algorithmName,
    linearValue,
    newtonValue,
  )
where

import Types

data Algorithm
  = AlgLinear
  | AlgNewton Int
  deriving (Eq, Show)

algorithmName :: Algorithm -> String
algorithmName AlgLinear = "linear"
algorithmName (AlgNewton _) = "newton"

linearValue :: [Point] -> Double -> Maybe Double
linearValue [] _ = Nothing
linearValue [_] _ = Nothing
linearValue (p1 : p2 : rest) x
  | x < x1 = Nothing
  | x <= x2 && x2 > x1 = Just (y1 + (y2 - y1) * (x - x1) / (x2 - x1))
  | otherwise = linearValue (p2 : rest) x
  where
    x1 = px p1
    y1 = py p1
    x2 = px p2
    y2 = py p2

newtonValue :: Int -> [Point] -> Double -> Maybe Double
newtonValue n points x
  | n <= 0 = Nothing
  | length points < n = Nothing
  | otherwise = do
      window <- pickWindow n x points
      pure (evaluateNewton window x)

pickWindow :: Int -> Double -> [Point] -> Maybe [Point]
pickWindow n x pts
  | length pts < n = Nothing
  | otherwise = Just (take n (drop start pts))
  where
    xs = map px pts
    len = length pts
    pos = length (takeWhile (<= x) xs)
    half = n `div` 2
    rawStart = pos - half
    maxStart = len - n
    start
      | rawStart < 0 = 0
      | rawStart > maxStart = maxStart
      | otherwise = rawStart

evaluateNewton :: [Point] -> Double -> Double
evaluateNewton pts x = sum (zipWith term coeffs [0 ..])
  where
    xs = map px pts
    ys = map py pts
    coeffs = dividedDifferences xs ys

    term c idx = c * product [x - xs !! j | j <- [0 .. idx - 1]]

dividedDifferences :: [Double] -> [Double] -> [Double]
dividedDifferences xs ys = go ys 0
  where
    n = length xs

    go [] _ = []
    go current@(c : _) level
      | level >= n = []
      | otherwise =
          let next =
                [ (current !! (i + 1) - current !! i)
                    / (xs !! (i + level + 1) - xs !! i)
                | i <- [0 .. n - level - 2]
                ]
           in c : go next (level + 1)