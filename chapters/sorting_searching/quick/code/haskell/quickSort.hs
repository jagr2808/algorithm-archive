quickSort []  = []
quickSort [x] = [x]
quickSort (pivot:xs) = quickSort firstHalf ++ pivot : quickSort secondHalf
  where (firstHalf, secondHalf) = partition (<x) xs
        --partition can be imported from Data.List, but her's an implementation
        partition f xs = (filter f xs, filter (not . f) xs) 
