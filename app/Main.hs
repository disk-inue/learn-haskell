module Main (main) where

import qualified Data.Map.Strict as Map
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

data TodoStatus = Done | Todo deriving (Show)

data TodoItem = TodoItem {itemId :: Int, title :: String, status :: TodoStatus} deriving (Show)

todo :: IO ()
todo = do
  putStrLn "start doto"
  putStrLn "q is end todo"
  putStrLn "select menu : 1. add, 2. edit, 3. done, 4. delete, 5. list"
  let todoMap = Map.empty :: Map.Map Int TodoItem
  let addMap = Map.insert 1 TodoItem {itemId = 1, title = "hoge", status = Done} todoMap
  print addMap
  editInputNumber <- prompt "number >"
  let maybeEditNumber = readMaybe editInputNumber :: Maybe Int
  case maybeEditNumber of
    Nothing -> return ()
    Just editNumber -> do
      editInputTitle <- prompt "title >"
      let editMap = Map.adjust (\value -> value {title = editInputTitle}) editNumber addMap
      print editMap

      removeInputNumber <- prompt "remove number >"
      let maybeRemoveNumber = readMaybe removeInputNumber :: Maybe Int
      case maybeRemoveNumber of
        Nothing -> return ()
        Just removeNumber -> do
          let removeMap = Map.delete removeNumber editMap
          print removeMap
          return ()

main :: IO ()
main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"

  calculator
  todo
