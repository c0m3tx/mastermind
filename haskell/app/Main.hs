module Main where

import Mastermind (generateCode, playLoop)

main :: IO ()
main = do
  secretCode <- generateCode
  putStrLn "Welcome to Mastermind! Try to guess the 4-digit secret code!"
  playLoop secretCode 10

