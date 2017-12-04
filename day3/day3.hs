import Data.Tuple.Select

nextLowestSquareCoordinates :: Int -> (Int, Int, Int)
nextLowestSquareCoordinates n = (offset, -offset, last squares)
    where 
        squares = takeWhile (<=n) $ map (^2) [3,5..]
        offset =  length squares

manhattanDistance :: Int -> Int
manhattanDistance 1 = 0
manhattanDistance n = abs xn + abs yn
    where
        nCoord = moveToNext n coord $ x + 1 
        coord = nextLowestSquareCoordinates n
        x = sel1 coord
        xn = sel1 nCoord
        yn = sel2 nCoord

moveToNext :: Int -> (Int, Int, Int) -> Int -> (Int, Int, Int)
moveToNext n c rowLength
        | n == v = c
        | x == rowLength && y < rowLength = moveToNext n (x, y + 1, v + 1) rowLength
        | x > -rowLength && y == rowLength = moveToNext n (x - 1, y, v + 1) rowLength
        | x == -rowLength && y > -rowLength = moveToNext n (x, y - 1, v + 1) rowLength
        | x < rowLength && y == -rowLength = moveToNext n (x + 1, y, v + 1) rowLength
        | x == rowLength - 1 && y == -rowLength + 1 = moveToNext n (x + 1, y, v + 1) rowLength
                where x = sel1 c
                      y = sel2 c
                      v = sel3 c