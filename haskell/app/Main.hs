module Main where

import Mastermind (generateCode, playLoop)

main :: IO ()
main = do
  secretCode <- generateCode
  putStrLn $ "Welcome to Mastermind! Try to guess the 4-digit secret code (which is " ++ show secretCode ++ " for testing purposes)."
  playLoop secretCode 10

