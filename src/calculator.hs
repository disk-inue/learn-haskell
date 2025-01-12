import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  getLine

main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"
  inputLeftNumber <- prompt "number > "
  inputArithmetic <- prompt "four arithmetic operations(+, -, *, /) > "
  inputRightNumber <- prompt "number > "
  putStrLn (inputLeftNumber ++ " " ++ inputArithmetic ++ " " ++ inputRightNumber ++ " = ")
