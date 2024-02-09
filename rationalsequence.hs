parse :: String -> (Int, Int, Int)
-- parse _ = (0, 0, 0)
parse s = case words s of
  [a, b] -> case span (/= '/') b of
    (num, '/' : den) -> (read a, read num, read den)
    _ -> error "incorrect format"
  _ -> error "incorrect format"

upLeft n (p, q) = (p - n * q, q)

upRight (p, q) = (p, q - p)

downLeft (p, q) = (p + q, q)

downRight n (p, q) = (p, q + n * p)

compute :: (Int, Int) -> (Int, Int)
compute (p, 1) = (1, p + 1)
compute (p, q) =
  if (p < q)
    then (q, q - p)
    else downRight n (downLeft (upRight (upLeft n (p, q))))
  where
    n = p `div` q

solve :: IO ()
solve = do
  input <- getLine
  let (n, a, b) = parse input
  let (p, q) = compute (a, b)
  putStrLn $ show n ++ " " ++ show p ++ "/" ++ show q

iter :: Int -> IO ()
iter 1 = do
  solve
iter n = do
  solve
  iter (n - 1)

main :: IO ()
main = do
  input <- getLine
  let num_lines = read input :: Int
  iter num_lines
