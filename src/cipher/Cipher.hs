module Cipher (caesar, unCaesar, vigenere, unVigenere) where

import Data.Char (ord, chr)

type Key = String

minOrd :: Int
minOrd = ord minBound

maxOrd :: Int
maxOrd = ord maxBound

charFromInt :: Int -> Char
charFromInt n = chr $ mod (n - minOrd) (maxOrd + 1 - minOrd) + minOrd

shift :: Int -> Char -> Char
shift n c = charFromInt $ ord c + n

caesar :: Int -> String -> String
caesar = map . shift

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

emptyDef :: Eq a => [a] -> [a] -> [a]
emptyDef def x = if x == [] then def else x

offsets :: Key -> [Int]
offsets = map (\c -> ord c - minOrd) . concat . repeat . emptyDef [minBound]

vigenere :: Key -> String -> String
vigenere key = zipWith shift (offsets key)

unVigenere :: Key -> String -> String
unVigenere key = zipWith shift (map negate $ offsets key)
