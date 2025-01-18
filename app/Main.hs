module Main (main) where

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
  case maybeLeftNumber of
    Nothing -> return ()
    Just leftNumber -> do
      inputArithmetic <- prompt "four arithmetic operations(+, -, *, /) > "
      inputRightNumber <- prompt "number > "
      let maybeRightNumber = readMaybe inputRightNumber :: Maybe Double
      case maybeRightNumber of
        Nothing -> return ()
        Just rightNumber -> do
          let result = calculate leftNumber inputArithmetic rightNumber
          putStrLn (inputLeftNumber ++ " " ++ inputArithmetic ++ " " ++ inputRightNumber ++ " = " ++ show result)
          calculator

main :: IO ()
main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"

  calculator
