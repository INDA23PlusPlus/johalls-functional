rev [] = []
rev (head : tail) = rev (tail) ++ head
