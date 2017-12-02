compute :: String -> Int
compute xs = computeSum (xs ++ [head xs])  0
    where computeSum [x] computedSum = computedSum
          computeSum (x:xs) computedSum 
            | x == (head xs) = computeSum xs (computedSum + read [x] :: Int)
            | otherwise = computeSum xs computedSum


compute2 xs = computeSum xs 0 (length xs `div` 2)
    where computeSum (x:xs) computedSum compareOffset
            | length xs < compareOffset = computedSum
            | x == xs !! (compareOffset - 1) = computeSum xs (computedSum + (read [x] :: Int) * 2) compareOffset
            | otherwise = computeSum xs computedSum compareOffset