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

-- | List of form [(Outcome, Probability)]
variance :: Floating a => [(a, a)] -> a
variance xs = sum $ map (\(x, y) -> y * (x + n x') ** 2) xs
  where
    x' = mean xs

facarr :: Num a => Array Int a
facarr = listArray (0, floor 1e3) (1 : map (\x -> fromIntegral (x + 1) * facarr ! x) [0..])

fac = (facarr !)
nPr x y = fac x * r (fac (max 0 $ x + n y))
