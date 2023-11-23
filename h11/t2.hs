-- func1 AH autumn 2023
myReadMaybe :: (Read a) => String -> Maybe a
myReadMaybe s =
    case reads s of
        [(val, "")] -> Just val
        _ -> Nothing

        
-- Parse an expression into its components
parseExpression :: String -> Maybe (Int, Char, Int)
parseExpression expr = case words expr of
    [a, [op], b] -> do
        x <- myReadMaybe a
        y <- myReadMaybe b
        return (x, op, y)
    _ -> Nothing
    

-- Perform the calculation of components
performCalculation :: (Int, Char, Int) -> Maybe Int
performCalculation (x, op, y) = case op of
    '+' -> Just (x + y)
    '-' -> Just (x - y)
    '*' -> Just (x * y)
    _ -> Nothing


calculate :: [String] -> [String]
calculate = map $ \expr ->
    case parseExpression expr >>= performCalculation of
        Just result -> show result
        Nothing -> "I cannot calculate that"
