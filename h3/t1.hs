-- func1 AH autumn 2023
-- 3.1.1 - Maximum heart rate

maxhr :: Float -> Float
maxhr x 
    | x > 40    = 207 - 0.7 * x 
    | otherwise = 220 - x
