{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cipher where

import Data.Char (ord, chr)

type Key = String

newtype CharInt = CharInt { unCharInt :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Bounded CharInt
  where
    minBound = CharInt $ ord (minBound :: Char)
    maxBound = CharInt $ ord (maxBound :: Char)

wrapToBounds :: (Bounded a, Integral a) => a -> a
wrapToBounds n = mod (n - minBound) (maxBound + 1 - minBound) + minBound

charFromInt :: Int -> Char
charFromInt = chr . unCharInt . wrapToBounds . CharInt

shift :: Int -> Char -> Char
shift n c = charFromInt $ ord c + n

caesar :: Int -> String -> String
caesar = map . shift

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

offsets :: Key -> [Int]
offsets = map (\c -> ord c - ord minBound) . concat . repeat . zeroIfEmpty
  where
    zeroIfEmpty x = if x == "" then [minBound] else x

vigenere :: Key -> String -> String
vigenere key = zipWith shift (offsets key)

unVigenere :: Key -> String -> String
unVigenere key = zipWith shift (map negate $ offsets key)
