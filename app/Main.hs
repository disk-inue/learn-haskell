module Main (main) where

import Control.Monad (when)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  getLine

calculate :: Double -> String -> Double -> Double
calculate leftNumber arithmetic rightNumber
  | arithmetic == "+" = leftNumber + rightNumber
  | arithmetic == "-" = leftNumber - rightNumber
  | arithmetic == "*" = leftNumber * rightNumber
  | arithmetic == "/" = leftNumber / rightNumber
  | otherwise = error "invalid arithmetic"

calculator :: IO ()
calculator = do
  inputLeftNumber <- prompt "number > "
  let maybeLeftNumber = readMaybe inputLeftNumber :: Maybe Double
  let (leftNumber, leftShouldContinue) = case maybeLeftNumber of
        Just parsedInput -> (parsedInput, True)
        Nothing -> (0.0, False)

  inputArithmetic <- prompt "four arithmetic operations(+, -, *, /) > "

  inputRightNumber <- prompt "number > "
  let maybeRightNumber = readMaybe inputRightNumber :: Maybe Double
  let (rightNumber, rightShouldContinue) = case maybeRightNumber of
        Just parsedInput -> (parsedInput, True)
        Nothing -> (0.0, False)

  let result = calculate leftNumber inputArithmetic rightNumber
  when (leftShouldContinue || rightShouldContinue) $
    putStrLn (inputLeftNumber ++ " " ++ inputArithmetic ++ " " ++ inputRightNumber ++ " = " ++ show result)

main :: IO ()
main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"

  calculator
