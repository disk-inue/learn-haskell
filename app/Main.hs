module Main (main) where

import Data.Map (Map)
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
  let todoMap = Map.empty :: Map.Map Int TodoItem

  putStrLn "select menu : 1. add, 2. edit, 3. done, 4. delete, 5. list"
  addInputTitle <- prompt "title >"
  let addMap = addTodo (Map.size todoMap) TodoItem {itemId = 1, title = addInputTitle, status = Todo} todoMap
  print addMap
  editInputNumber <- prompt "number >"
  let maybeEditNumber = readMaybe editInputNumber :: Maybe Int
  case maybeEditNumber of
    Nothing -> return ()
    Just editNumber -> do
      editInputTitle <- prompt "title >"
      let editMap = editTodo editNumber editInputTitle addMap
      print editMap

      doneInputNumber <- prompt "number >"
      let maybeDoneNumber = readMaybe doneInputNumber :: Maybe Int
      case maybeDoneNumber of
        Nothing -> return ()
        Just doneNumber -> do
          let doneMap = doneTodo doneNumber editMap
          print doneMap
          removeInputNumber <- prompt "remove number >"
          let maybeRemoveNumber = readMaybe removeInputNumber :: Maybe Int
          case maybeRemoveNumber of
            Nothing -> return ()
            Just removeNumber -> do
              let removeMap = Map.delete removeNumber editMap
              print removeMap
              return ()

addTodo :: Int -> TodoItem -> Map Int TodoItem -> Map Int TodoItem
addTodo key value targetMap = Map.insert key value targetMap

editTodo :: Int -> String -> Map Int TodoItem -> Map Int TodoItem
editTodo targetKey editTitle targetMap = Map.adjust (\value -> value {title = editTitle}) targetKey targetMap

doneTodo :: Int -> Map Int TodoItem -> Map Int TodoItem
doneTodo targetKey targetMap = Map.adjust (\value -> value {status = Done}) targetKey targetMap

main :: IO ()
main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"

  calculator
  todo
