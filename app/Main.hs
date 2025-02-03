module Main (main) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import System.IO (IO, hFlush, stdout, print, putStr, putStrLn, getLine)
import System.Random (newStdGen, randomR, StdGen)
import Text.Read (readMaybe)
import Prelude (String, Int, Double,  Maybe (Just, Nothing), Show, return, (++), error, ($), (==), (+),(-),(/),(*),show,otherwise)

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
  todoExec todoMap

todoExec :: Map Int TodoItem -> IO ()
todoExec targetMap = do
  putStrLn "select menu : 1. add, 2. edit, 3. done, 4. delete, 5. list"
  menuInput <- prompt ">"
  let maybeMenuNumber = readMaybe menuInput :: Maybe Int
  case maybeMenuNumber of
    Nothing -> return ()
    Just menuNumber -> do
      todoSelectMenu menuNumber targetMap
      return ()

todoSelectMenu :: Int -> Map Int TodoItem -> IO ()
todoSelectMenu menuNumber todoMap = do
  case menuNumber of
    1 -> do
      addInputTitle <- prompt "title >"
      let addMap = addTodo (Map.size todoMap) TodoItem {itemId = 1, title = addInputTitle, status = Todo} todoMap
      print addMap
      todoExec addMap
    2 -> do
      editInputNumber <- prompt "edit number >"
      let maybeEditNumber = readMaybe editInputNumber :: Maybe Int
      case maybeEditNumber of
        Nothing -> return ()
        Just editNumber -> do
          editInputTitle <- prompt "title >"
          let editMap = editTodo editNumber editInputTitle todoMap
          print editMap
          todoExec editMap
    3 -> do
      doneInputNumber <- prompt "number >"
      let maybeDoneNumber = readMaybe doneInputNumber :: Maybe Int
      case maybeDoneNumber of
        Nothing -> return ()
        Just doneNumber -> do
          let doneMap = doneTodo doneNumber todoMap
          print doneMap
          todoExec doneMap
    4 -> do
      removeInputNumber <- prompt "remove number >"
      let maybeRemoveNumber = readMaybe removeInputNumber :: Maybe Int
      case maybeRemoveNumber of
        Nothing -> return ()
        Just removeNumber -> do
          let removeMap = Map.delete removeNumber todoMap
          print removeMap
          todoExec removeMap
    5 -> do
      print todoMap
      todoExec todoMap
    _ -> return ()

addTodo :: Int -> TodoItem -> Map Int TodoItem -> Map Int TodoItem
addTodo key value targetMap = Map.insert key value targetMap

editTodo :: Int -> String -> Map Int TodoItem -> Map Int TodoItem
editTodo targetKey editTitle targetMap = Map.adjust (\value -> value {title = editTitle}) targetKey targetMap

doneTodo :: Int -> Map Int TodoItem -> Map Int TodoItem
doneTodo targetKey targetMap = Map.adjust (\value -> value {status = Done}) targetKey targetMap

dice :: IO ()
dice = do
  putStrLn "start dice"
  gen <- newStdGen
  let (diceNumber, _) = randomR (1, 6) gen :: (Int, StdGen)
  putStrLn $ "dice number " ++ show diceNumber

main :: IO ()
main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"

  calculator
  todo
  dice
