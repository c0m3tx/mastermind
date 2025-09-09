module Mastermind (playLoop, generateCode) where

import Data.Bool (bool)
import Data.Char (digitToInt, isDigit)
import Data.List (intersect, nub)
import System.Random (newStdGen, uniformShuffleList)

generateCode :: IO [Int]
generateCode = do
  gen <- newStdGen
  return $ take 4 $ fst $ uniformShuffleList [0 .. 9] $ gen

playLoop :: [Int] -> Int -> IO ()
playLoop secretCode 0 = putStrLn $ "Game Over! You did not guess the code (was " ++ show secretCode ++ ")."
playLoop secretCode attempts = do
  putStrLn $ "You have " ++ show attempts ++ " attempts left."
  prompt >>= guess attempts secretCode

prompt :: IO [Int]
prompt = do
  putStrLn "Enter your 4 digits guess: "
  guess <- nub . map digitToInt . filter isDigit <$> getLine
  if length guess /= 4
    then
      putStrLn "Invalid input (4 digits, must be unique)" >> prompt
    else
      return guess

guess :: Int -> [Int] -> [Int] -> IO ()
guess _ secret guess | secret == guess = do
  putStrLn "Result: XXXX"
  putStrLn "Well done! You won!"
guess attempts secret guess =
  let xs = xes secret guess
      os = oes secret guess
   in do
        putStrLn $ "Result: " ++ (replicate xs 'X') ++ (replicate os 'O')
        playLoop secret (attempts - 1)

xes :: [Int] -> [Int] -> Int
xes secret guess = length $ filter (uncurry (==)) $ zip secret guess

-- got many possible solutions for this...
-- xes secret guess = length [a | (a, b) <- zip secret guess, a == b]

oes :: [Int] -> [Int] -> Int
oes secret guess =
  length (intersect secret guess) - (xes secret guess)
