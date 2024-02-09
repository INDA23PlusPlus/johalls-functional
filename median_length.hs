import Data.Bits
import Data.List

median_even :: [Int] -> Double
median_even [] = error "cant get median of empty list"
median_even list = (fromIntegral ((sorted !! mid) + (sorted !! (mid - 1)))) / 2.0
  where
    sorted = sort list
    mid = length list `div` 2

median_odd :: [Int] -> Double
median_odd list = fromIntegral (sorted !! mid)
  where
    sorted = sort list
    mid = length list `div` 2

median :: [Int] -> Double
median list =
  if even (length list)
    then median_even list
    else median_odd list

median_lengths :: [String] -> Double
median_lengths list = median (map length list)
