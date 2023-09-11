-- func1 AH autumn 2023
-- 3.1.3 - List of strings

-- returns a list of strings which begin or end with char
headOrLast :: [String] -> Char -> [String]
headOrLast list char = filter (startsOrEndsWith char) list

-- checks if the string begins or ends with char
startsOrEndsWith :: Char -> String -> Bool
startsOrEndsWith char string
    | head string == char   = True
    | last string == char   = True
    | otherwise             = False