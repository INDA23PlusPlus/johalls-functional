median :: [Int] -> Int
median [] = error "cant find median of empty list"
median [a] = a
median [a, b] = (a + b) `div` 2
median [a, b, c] = (a + b + c) - (min a (min b c)) - (max a (max b c))

pivot :: [Int] -> Int
pivot [] = error "cant find pivot of empty list"
pivot a = ((a !! 0) + (a !! (n `div` 2)) + (a !! (n - 1))) `div` 3
  where
    n = length a

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [a] = [a]
quicksort [a, b] = [min a b, max a b]
quicksort a =
  let p = pivot a
   in quicksort (filter (\x -> x < p) a) ++ (filter (\x -> x == p) a) ++ quicksort (filter (\x -> x > p) a)