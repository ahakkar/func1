-- func1 AH autumn 2023

validate :: String -> Bool
validate str 
  | length str /= 18            = False
  | take 2 str /= "FI"          = False
  | not (all isDigit restStr)   = False
  | otherwise                   = checkIBAN str
  where
    isDigit c = c >= '0' && c <= '9'
    restStr = take 16 (drop 2 str)

-- strtoi(str) % 97 should be 1...
checkIBAN :: String -> Bool
checkIBAN str = read(replaceChars str) `mod` 97 == 1

-- drop first four, add 1518 to end, add 3rd and 4th dropped char to end of str
replaceChars :: String -> String
replaceChars str = drop 4 str ++ "1518" ++ take 2 (drop 2 str)


