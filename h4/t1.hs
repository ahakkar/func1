-- func1 AH autumn 2023

nextIsGreater2 :: [Int] -> IO()  -- returns IO... in other words.. prints stuff
nextIsGreater2 [] = return ()    -- base case - an empty list
nextIsGreater2 (firstElement : remainingElements) = do
  print firstElement
  nextIsGreater2 remainingElements



nextIsGreater :: [Int] -> [Int]
nextIsGreater [] = []           -- empty list
nextIsGreater [_] = []          -- only one element (nothing to compare)
nextIsGreater (first : second : rest)                      -- breaks the list to 3 variables
  | first < second = first : nextIsGreater (second : rest) -- check if next element is larger, and keep it if true
  | otherwise = nextIsGreater (second : rest)              -- otherwise discard the element in first and continue recursion