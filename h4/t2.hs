-- func1 AH autumn 2023


nextIsGreater :: [Int] -> [Int]
nextIsGreater [] = []           -- empty list
nextIsGreater [_] = []          -- only one element (nothing to compare)
nextIsGreater (first : second : rest)                      -- breaks the list to 3 variables
  | first < second = first : nextIsGreater (second : rest) -- check if next element is larger, and keep it if true
  | otherwise = nextIsGreater (second : rest)              -- otherwise discard the element in first and continue recursion

onlyDigits :: String -> Bool
onlyDigits [] = False                   -- empty string
onlyDigits [x] = x >= '0' && x <= '9'   -- handle last digit
onlyDigits (first : rest)               -- break the 1st index to a separate variable
    | first >= '0' && first <= '9' = onlyDigits rest        -- why not: isDigit first hlint(refract: use isDigit)
    | otherwise                    = False                  -- well because these have to be done without IMPORTS..

{--
imXort Data.Char (isDigit)

onlyDigits2 :: String -> Bool
onlyDigits2 [] = False          -- empty string
onlyDigits2 [x] = isDigit x     -- handle last digit
onlyDigits2 (first : rest)               -- break the 1st index to a separate variable
    | isDigit first = onlyDigits2 rest   
    | otherwise     = False              
--}