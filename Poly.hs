module Poly
  ( module Lib
  , module Poly
  )
where

import Lib

import Data.List
import Data.Function


data Poly a = Poly { factor :: a, power :: a } deriving (Show, Eq, Functor)
newtype Polynom a = P { unP :: [Poly a] } deriving (Show, Eq, Functor)

polyMerge :: (Num a, Ord a) => Polynom a -> Polynom a
polyMerge (P xs) = P $ map (foldl1 (\(Poly fac pow) (Poly fac' _) -> Poly (fac + fac') pow)) grouped
  where
    fun f = on f power
    sorted = sortBy (fun compare) xs
    grouped = groupBy (fun (==)) sorted

instance  (Num a, Ord a) => Num (Polynom a) where
  P a + P b = polyMerge $ P $ a <> b
  negate (P b) = P $ map (\(Poly fac pow) -> Poly (negate fac) pow) b
  a * b = {- unnecessary ? -} polyMerge $ P [ Poly (facx * facy) (powx + powy) | Poly facx powx <- a', Poly facy powy <- b']
    where
     P a' = polyMerge a
     P b' = polyMerge b
  
  abs = undefined
  signum = undefined
  fromInteger x = P [Poly (fromInteger x) 0]

test = [a + b, a * b]
  where
    a = P [Poly 3 2, Poly 4 6] + 5
    b = P [Poly 5 2, Poly 2.5 3] + 3
