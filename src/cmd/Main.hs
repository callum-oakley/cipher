module Main where

import System.Environment (getArgs, getProgName)
import Data.List (find)

import Cipher (vigenere, unVigenere)

decodeFlag :: [String] -> Bool
decodeFlag = any (`elem` ["-d", "--decode"])

password :: [String] -> Maybe String
password = find ((/= '-') . head)

main :: IO ()
main = do
  args <- getArgs
  case password args of
    Nothing -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " [-d|--decode] PASSWORD"
    Just pw ->
      if decodeFlag args then
        interact $ unVigenere pw
      else
        interact $ vigenere pw
