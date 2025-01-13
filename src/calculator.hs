import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  getLine

calculate :: Double -> String -> Double -> Double
calculate leftNumber arithmetic rightNumber = do
  case arithmetic of
    "+" -> leftNumber + rightNumber
    "-" -> leftNumber - rightNumber
    "*" -> leftNumber * rightNumber
    "/" -> leftNumber / rightNumber

main = do
  putStrLn "start calculator"
  putStrLn "q is end calculator"

  inputLeftNumber <- prompt "number > "
  let leftNumber = read inputLeftNumber :: Double

  inputArithmetic <- prompt "four arithmetic operations(+, -, *, /) > "

  inputRightNumber <- prompt "number > "
  let rightNumber = read inputRightNumber :: Double

  let result = calculate leftNumber inputArithmetic rightNumber
  putStrLn (inputLeftNumber ++ " " ++ inputArithmetic ++ " " ++ inputRightNumber ++ " = " ++ show result)
