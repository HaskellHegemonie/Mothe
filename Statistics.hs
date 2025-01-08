module Statistics
  ( module Lib
  , module Statistics
  )
where
import Lib
import Data.Array
import Data.Function

mean :: Num a => [(a, a)] -> a
mean = sum . map (uncurry (*))

-- | List of form [(Probability, Outcome)]
variance :: Floating a => [(a, a)] -> a
variance xs = sum $ map (\(y, x) -> y * (x + n x') ** 2) xs
  where
    x' = mean xs

fac :: Num a => Array Int a
fac = listArray (0, floor 1e3) (1 : map (\x -> fromIntegral (x + 1) * fac ! x) [0..])

nPr x y = fac ! y * r (fac ! (max 0 $ y + n x))
