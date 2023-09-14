-- func1 AH autumn 2023

-- filter iterates through every index of the list
-- \c is the parameter currently being evaluated, like char a from index 0
-- the content after -> is the function, which evaluates either true or false
-- if the remainder is 0 then the char is divisible by n, thus the eval is True
-- otherwise the parameter's letter is filtered out from the a-z list
charsDivisibleBy :: Int -> [Char]
charsDivisibleBy 0 = []
charsDivisibleBy n = filter (\c -> ((fromEnum c - 96) `mod` n) == 0) ['a'..'z']


-- create a list of products, and filter the a-z list based on the list of products
charsProductOf :: [Int] -> [Char]
charsProductOf lst = chars
    where
        -- list of all products of two individual numbers from the lst param
        products = [x * y | x <- lst, y <- lst, x < y] 
        -- chars where the number presentation equals a number from pairs list
        chars = filter (\c -> (fromEnum c - 96) `elem` products) ['a'..'z']

numsList :: [Int] -> [Int]
numsList lst = lst2
    where
        -- list of all products of two individual numbers from the lst param
        lst2 = [x * y | x <- lst, y <- lst, x < y] 