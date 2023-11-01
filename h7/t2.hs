-- func1 AH autumn 2023
import Data.List (elemIndex)

commonSubstring :: String -> String -> String
commonSubstring [] _ = []
commonSubstring _ [] = []
commonSubstring (first:rest) str2 = 
    -- try using elemindex to avoid modifying the original string
    case elemIndex first str2 of
    Just index -> 
        -- create new string which starts from the next index from found character
        first : commonSubstring rest (drop (index + 1) str2)
    Nothing -> 
        commonSubstring rest str2

commonSubstring2 :: String -> String -> String
-- ignore empty strings and chars
commonSubstring2 [] _ = []
commonSubstring2 _ [] = []
commonSubstring2 (first:rest) str2 =
  if first `elem` str2
    -- adds characters to output string until the first character is not in the second string
    then first : commonSubstring2 rest (dropWhile (/= first) str2)
    -- otherwise continue recursion
    else commonSubstring2 rest str2