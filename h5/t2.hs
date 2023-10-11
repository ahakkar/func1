-- func1 AH autumn 2023

distance1 :: String -> String -> Float
distance1 str1 str2
    | null str1 && null str2 = 0.0 -- empty strings - no distance
    -- use the provided distance formula
    | otherwise = fromIntegral (count1 + count2) / fromIntegral (length str1 + length str2)
    where
        count1 = length [ c | c <- str1, not (c `elem` str2) ]  -- create a new list and count the indexes
        count2 = length [ c | c <- str2, not (c `elem` str1) ]  



distance2 :: String -> String -> Float
distance2 str1 str2
    | null str1 && null str2 = 0.0 -- empty strings - no distance between them
    -- use the provided distance formula
    | otherwise = fromIntegral (count1 + count2) / fromIntegral (length str1 + length str2)
    where
        count1 = length [ c | c <- str1, not (c `elem` ['0'..'9']) ] -- exclude digits this time
        count2 = length [ c | c <- str2, not (c `elem` ['0'..'9']) ]