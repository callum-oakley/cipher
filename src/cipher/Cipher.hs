{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cipher where

import Data.Char (ord, chr)

type Key = String

newtype AsciiInt = AsciiInt { unAsciiInt :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Bounded AsciiInt
  where
    minBound = AsciiInt 0x01
    maxBound = AsciiInt 0x7E

wrapToBounds :: (Bounded a, Integral a) => a -> a
wrapToBounds n = mod (n - minBound) (maxBound + 1 - minBound) + minBound

asciiFromInt :: Int -> Char
asciiFromInt = chr . unAsciiInt . wrapToBounds . AsciiInt

shift :: Int -> Char -> Char
shift n c = asciiFromInt $ ord c + n

caesar :: Int -> String -> String
caesar = map . shift

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

offsets :: Char -> String -> [Int]
offsets zero = map (\c -> ord c - ord zero) . concat . repeat

vigenere :: Char -> Key -> String -> String
vigenere zero key = zipWith shift (offsets zero key)

unVigenere :: Char -> Key -> String -> String
unVigenere zero key = zipWith shift (map negate $ offsets zero key)
