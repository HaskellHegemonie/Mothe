{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Poly
  ( module Lib
  , module Poly
  )
where

import Lib

import Data.List
import Data.Function
import GHC.IsList
import Data.Bifunctor
import Text.Printf
import Control.Arrow

import Debug.Trace

data Poly a = Poly { factor :: a, power :: a } deriving (Show, Eq, Functor)


instance (Num a, Ord a) => Num (Poly a) where
  Poly f p + Poly f' p' = if p == p' then Poly (f + f') p else undefined
  Poly f p * Poly f' p' = Poly (f * f') (p + p')
  abs = undefined
  signum = undefined
  negate (Poly f p) = Poly (n f) p
  fromInteger x = Poly (fromInteger x) 0

instance (Fractional a, Ord a) => Fractional (Poly a) where
  fromRational x = Poly (fromRational x) 0
  recip (Poly f p) = Poly (r f) (n p)

data Polynom a = P { unP :: [Poly a], polyRem :: [Poly a] } deriving (Show, Eq, Functor)

instance Num a => IsList (Polynom a) where
  type Item (Polynom a) = a
  fromList lx = P (map (\(i, x) -> Poly x (fromIntegral i)) (zip (reverse [0..l]) lx)) []
    where
      l = length lx + n 1
  toList (P xs _) = map factor xs

polyMerge :: (Num a, Ord a) => Polynom a -> Polynom a
polyMerge (P xs rem) = P (map (foldl1 (\(Poly fac pow) (Poly fac' _) -> Poly (fac + fac') pow)) grouped) rem
  where
    fun f = on f power
    sorted = sortBy (fun (flip compare)) xs
    grouped = groupBy (fun (==)) sorted

instance  (Num a, Ord a) => Num (Polynom a) where
  P a rema + P b remb = polyMerge $ P (a <> b) $ rema <> remb
  negate (P b remb) = P (map (\(Poly fac pow) -> Poly (negate fac) pow) b) remb
  a * b = {- unnecessary ? -} polyMerge $ P [ Poly (facx * facy) (powx + powy) | Poly facx powx <- a', Poly facy powy <- b']  (rema <> remb)
    where
     P a' rema = polyMerge a
     P b' remb = polyMerge b
  
  abs = undefined
  signum = undefined
  fromInteger x = P [Poly (fromInteger x) 0] []

instance (Fractional a, Ord a) => Fractional (Polynom a) where
  (/) = undefined
  recip = undefined
  fromRational x = P [Poly (fromRational x) 0] []

polyDiv (P [] _) _ = P [] []
polyDiv a b
  | pow < pow' = P [] $ filter ((/= 0) . factor) $ unP $ P [Poly newFac pow] [] + P as []
  | fac == 0 = polyDiv (P as rema) b'
  | otherwise = {- traceShow (P as rema + n (res * P bs remb)) $ -}  res + polyDiv (P as rema + n (res * P bs remb)) b'
    where
        (P (Poly fac pow : as) rema, b'@(P (Poly fac' pow' : bs) remb)) = (polyMerge a, polyMerge b)
        newFac = fac * r fac'
        res = P [Poly newFac (pow + n pow')] []

-- Examples
a0 = polyDiv [1, n 6, 9, n 4] [1, n 1]
a1 = polyDiv [2, 7, 3] [1, 3]
a2 = polyDiv [1, 0, 2, n 12] [1, 0, 1]
a3 = polyDiv [1, 0, n 7, 4, n 6] [1, 3]
wiki = polyDiv [1, n 12, 0, n 42] ([1, n 1] * [1, n 1])
