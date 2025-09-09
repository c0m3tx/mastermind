module Mastermind where

import System.Random (uniformShuffleList, newStdGen)
import Data.Char (isDigit, digitToInt)

generateCode :: IO [Int]
generateCode = do 
  gen <- newStdGen
  return $ take 4 $ fst $ uniformShuffleList [0..9] $ gen

main :: IO ()
main = do
  secretCode <- generateCode
  putStrLn $ "Welcome to Mastermind! Try to guess the 4-digit secret code (which is " ++ show secretCode ++ " for testing purposes)."
  playLoop secretCode 10

playLoop :: [Int] -> Int -> IO ()
playLoop _ 0 = putStrLn "Game Over! You did not guess the code."
playLoop secretCode attempts = do
  putStrLn $ "You have " ++ show attempts ++ " attempts left. Enter your guess (4 digits):"
  input <- getLine
  let guess = map digitToInt (filter isDigit $ input)   
  handleGuess secretCode guess attempts
    
handleGuess :: [Int] -> [Int] -> Int -> IO () 
handleGuess secret guess attempts
  | length guess /= 4 = do
      putStrLn "Invalid input. Please enter exactly 4 digits."
      playLoop secret attempts
  | xs == 4 = do
    putStrLn $ "Well done! You discovered the secret!"
  | otherwise = do
    putStrLn $ "Result: " ++ show xs ++ " x(s) and " ++ show os ++ " o(s)."
    playLoop secret (attempts - 1)

  where xs = xes secret guess
        os = oes secret guess
  
xes :: [Int] -> [Int] -> Int
xes secret guess = length $ filter (uncurry (==)) $ zip secret guess

oes :: [Int] -> [Int] -> Int
oes secret guess = 
  let os = length $ filter (flip any secret . (==)) guess
      xs = xes secret guess
  in os - xs
