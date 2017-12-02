compute :: String -> Int
compute xs = compute_sub (xs ++ [head xs])  0
    where compute_sub [x] computedSum = computedSum
          compute_sub (x:xs) computedSum 
            | x == (head xs) = compute_sub xs (computedSum + read [x] :: Int)
            | otherwise = compute_sub xs computedSum