-- |
-- Module      :  Languages.Rhythmicity
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Allows to evaluate (approximately, so better to say, to estimate) the
-- rhythmicity metrices for the text (usually, the poetic one).

{-# LANGUAGE BangPatterns #-}

module Languages.Rhythmicity where

import GHC.Int

maxPosition2 :: (RealFrac a) => [a] -> a
maxPosition2 xs
 | null xs = 0.0
 | otherwise = if mx2 == 0.0 then 2.0 * abs (maxP21 xs 0) else abs (maxP21 xs 0 / mx2)
     where maxP21 (x:y:ys) !acc1 = maxP21 ys (if x < y then (acc1 + 1)::Int16 else (acc1 - 1)::Int16)
           maxP21 _ !acc1 = fromIntegral acc1
           maxP22 (x:y:ys) !acc1 = maxP22 (y:ys) (if x < y then (acc1 + 1)::Int16 else (acc1 - 1)::Int16)
           maxP22 _ !acc1 = fromIntegral acc1
           !mx2 = maxP22 xs (0::Int16)

posMaxIn3
  :: (Ord a) => a
  -> a
  -> a
  -> Int16
posMaxIn3 x y z 
 | x < y = if y < z then 3 else 2
 | x < z = 3
 | otherwise = 1

maxPosition3 :: RealFrac a => [a] -> a
maxPosition3 xs
  | null xs = 0.0
  | length xs `rem` 3 == 0 = 3.0 * fromIntegral (go (h xs) ((0, 0, 0)::(Int16,Int16,Int16)))
  | otherwise = fromIntegral (go xs ((0, 0, 0)::(Int16,Int16,Int16)))
      where h (x:y:z:ys) = posMaxIn3 x y z:h ys
            h _ = []
            go [] (!acc21,!acc22,!acc23)
              | acc21 > acc22 = if acc21 > acc23 then acc21 else acc23
              | acc22 > acc23 = acc22
              | otherwise = acc23
            go (x:zs) (!acc21,!acc22,!acc23) = go zs (h1 x (acc21,acc22,acc23))
            h1 !x (!t,!u,!w)
              | x == 1 = (t + (1::Int16), u, w)
              | x == 2 = (t, u + (1::Int16), w)
              | otherwise = (t,u,w + (1::Int16))

evalRhythmicity23 :: (RealFrac a, Floating a) => [a] -> a
evalRhythmicity23 xs = maxPosition2 xs * maxPosition2 xs + maxPosition3 xs * maxPosition3 xs

evalRhythmicity23K
  :: (RealFrac a, Floating a) => a
  -> a
  -> [a]
  -> a
evalRhythmicity23K k2 k3 xs = k2 * maxPosition2 xs * maxPosition2 xs + k3 * maxPosition3 xs * maxPosition3 xs
