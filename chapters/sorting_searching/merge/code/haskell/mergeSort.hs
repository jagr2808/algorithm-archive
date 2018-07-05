mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where (firstHalf, secondHalf) = splitAt middle xs
        middle                  = (length xs) `div` 2
        merge xs []             = xs
        merge [] xs             = xs
        merge (x:xs) (y:ys)
          |x < y                = x : merge xs (y:ys)
          |otherwise            = y : merge (x:xs) ys
