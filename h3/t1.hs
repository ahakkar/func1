-- func1 AH autumn 2023

-- initial solution
credits :: (Char, Int) -> (Char, Int) -> Int
credits (char1, int1) (char2, int2)
    | (char1 == 's' && int1 == 14) || (char2 == 's' && int2 == 14)  = 14
    | (char1 == char2) && ((int1-1 == int2) || (int1+1 == int2))    = 8
    | int1 == int2                                                  = 6
    | (int1-1 == int2) || (int1+1 == int2)                          = 4
    | char1 == char2                                                = 2
    | otherwise                                                     = 0

-- alternative way to use wildcards _ and absolute differences with abs
credits2 :: (Char, Int) -> (Char, Int) -> Int
credits2 ('s', 14) _ = 14
credits2 _ ('s', 14) = 14
credits2 (char1, int1) (char2, int2)
    | char1 == char2 && abs (int1 - int2) == 1  = 8
    | int1 == int2                              = 6
    | abs (int1 - int2) == 1                    = 4
    | char1 == char2                            = 2
    | otherwise                                 = 0
