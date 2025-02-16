import System.IO (hFlush, stdout)

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

main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"

  inputLeftNumber <- prompt "number > "
  if inputLeftNumber == "q"
    then return ()
    else do
      let leftNumber = read inputLeftNumber :: Double
      inputArithmetic <- prompt "four arithmetic operations(+, -, *, /) > "
      if inputArithmetic == "q"
        then return ()
        else do
          inputRightNumber <- prompt "number > "
          if inputRightNumber == "q"
            then return ()
            else do
              let rightNumber = read inputRightNumber :: Double
              let result = calculate leftNumber inputArithmetic rightNumber
              putStrLn (inputLeftNumber ++ " " ++ inputArithmetic ++ " " ++ inputRightNumber ++ " = " ++ show result)
              main
