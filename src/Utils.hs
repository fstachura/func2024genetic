module Utils (
    numToBits,
    numToMinBits,
    pad,
    bitsToNum,
    sign,
    flipBit
) where

import Debug.Trace
import Data.Bits

-- TODO still very slow
numToMinBits :: (Bits a, Eq a, Integral a, Num a) => a -> [Bool]
numToMinBits n = reverse (numToBits' n)
            where numToBits' 0 = []
                  numToBits' n | (testBit n 0) = True:rest
                               | otherwise     = False:rest
                  rest = numToMinBits (n `shiftR` 1)

pad :: a -> Int -> [a] -> [a]
pad el n l = ((take (n-(length l))) (repeat el)) ++ l

numToBits :: (Bits a, Eq a, Integral a, Num a) => Int -> a -> [Bool]
numToBits p n = pad False p bits
    where bits = (numToMinBits (abs n))

bitsToNum :: (Num n) => [Bool] -> n
bitsToNum bits = bitsToNum' bits ((length bits)-1)
    where bitsToNum' (b:bs) p = (if (not b) then 0 else 2^p) + (bitsToNum' bs (p-1))
          bitsToNum' [] _ = 0

sign :: (Ord a, Num a) => a -> a
sign n  | n > 0  = 1
        | n == 0 = 0
        | n < 0  = -1

flipBit :: Int -> [Bool] -> [Bool]
flipBit n bits = (take n bits) ++ map not (drop n (take (n+1) bits)) ++ (drop (n+1) bits)

