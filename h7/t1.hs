-- func1 AH autumn 2023

clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters distanceFunc distanceLimit listOfStrings = 
    -- use list comprehension to create a list of lists
    [ findCloseStrings targetString | targetString <- listOfStrings ]
    where
        -- helper func which finds all strings close to the target string
        findCloseStrings target = 
            [ candidate | candidate <- listOfStrings, distanceFunc target candidate <= distanceLimit ]

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

distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter f d s ss = filter (\x -> f s x <= d) ss

distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y