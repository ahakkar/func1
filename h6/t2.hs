-- func1 AH autumn 2023

distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter f d s ss = filter (\x -> f s x <= d) ss

distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y