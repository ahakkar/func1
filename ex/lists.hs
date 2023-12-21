

main :: IO ()
main = do
    pm $ myLast [1,2,3,4]
    pm $ myLast ['x','y','z']
    putStrLn ""

    pm $ myButLast [1,2,3,4]
    pm $ myButLast ['a'..'z']
    putStrLn ""

    pm $ elementAt [1,2,3] 2
    pm $ elementAt "haskell" 5
    pm $ elementAt "haskell" 643
    putStrLn ""

    putStrLn $ myLength [123, 456, 789]
    putStrLn $ myLength "Hello, world!"
    putStrLn ""

    pm $ myReverse "A man, a plan, a canal, panama!"
    pm $ myReverse [1,2,3,4]
    putStrLn ""

    putStrLn $ show $ isPalindrome [1,2,3]
    putStrLn $ show $ isPalindrome "madamimadam"
    putStrLn $ show $ isPalindrome [1,2,4,8,16,8,4,2,1]

pm :: Show a => Maybe a -> IO ()
pm Nothing = return ()
pm (Just x) = print x

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast xs = Just $ last xs

myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast xs = Just $ xs !! (length xs - 2)

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt xs nth
    | nth > (length xs - 2) = Nothing
    | otherwise             = Just $ xs !! (nth-1)

myLength :: [a] -> String
myLength [] = show 0
myLength xs = show (length xs)

myReverse :: [a] -> Maybe [a]
myReverse [] = Nothing
myReverse xs = Just $ reverse xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs
    | reverse xs == xs = True
    | otherwise        = False

