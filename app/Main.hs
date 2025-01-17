module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "Guess the number!"

  putStrLn "Please enter your guess:"

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)
