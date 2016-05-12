module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

validLength :: Foldable t => t a -> Bool
validLength w = length w `elem` [5..9]

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter validLength aw)

randomWord :: WordList -> IO String
randomWord wordList = do
  randomIndex <- randomRIO (0, length wordList - 1)
  return $ wordList !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

main :: IO ()
main = undefined
