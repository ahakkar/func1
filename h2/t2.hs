-- func1 AH autumn 2023
-- 3.1.2 - Manhattan distance

points :: Int -> [(Int, Int)]
points n = 
    [ (x, y)                 -- desired output (list of tuples)
        | x <- [-n..n]       -- generates values of x
        , y <- [-n..n]       -- generates valeus of y
        , abs x + abs y <= n -- filters values
    ]