import Data.List.Split

getRow :: String -> [Int]
getRow = map read . words

findMaxDifference :: [Int] -> Int
findMaxDifference list = maximum list - minimum list

solvePart1 :: [[Int]] -> Int
solvePart1 rows = sum $ map findMaxDifference rows

makeDivisiblePair :: [Int] -> (Int, Int)
makeDivisiblePair list = head [(a, b) | a <- list, b <- list, a /= b, (a `mod` b) == 0]

dividePair :: (Int, Int) -> Int
dividePair (x, y) = x `div` y

solvePart2 :: [[Int]] -> Int
solvePart2 rows = sum $ map dividePair $ map makeDivisiblePair rows

main = do
    contents <- readFile "input"
    let rows = map getRow $ lines contents
    putStrLn ("Solution 1: " ++ (show $ solvePart1 rows))
    putStrLn ("Solution 2: " ++ (show $ solvePart2 rows))


