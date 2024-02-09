main :: IO ()
main = do
  input <- getLine
  putStrLn (show ((read input :: Double) * 0.09144))