-- func1 AH autumn 2023

gap :: (Char, Char) -> Int -> String -> Int
-- iterate through the string with helper function
gap (char1, char2) gap string = gapCounter char1 char2 gap string 0 

gapCounter :: Char -> Char -> Int -> String -> Int -> Int
gapCounter _ _ _ [] count = count   -- base case for empty string
gapCounter char1 char2 gap (first:rest) count
    -- return count if there's not enough string left to examine
    | length rest < (gap + 1) = count   
    -- if first index has the asked letter, and the 2nd letter is found index at gap, increment count
    | first == char1 && (rest !! gap) == char2 = gapCounter char1 char2 gap rest (count + 1)
    -- nothing found, continue iteration recursively
    | otherwise = gapCounter char1 char2 gap rest count