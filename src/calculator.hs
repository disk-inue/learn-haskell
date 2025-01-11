main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"
  putStrLn "number > "
  inputLeftNumber <- getLine
  putStrLn "four arithmetic operations(+, -, *, /) > "
  inputArithmetic <- getLine
  putStrLn "number > "
  inputRightNumber <- getLine
  putStrLn (inputLeftNumber ++ " " ++ inputArithmetic ++ " " ++ inputRightNumber ++ " = ")
