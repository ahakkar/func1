-- func1 AH autumn 2023

import qualified Data.Map

main :: IO ()
main = do
    input <- getLine
    putStrLn $ "> " ++ input
    case words input of
        ("quit": _) -> putStrLn "bye"
        ("encode":args) -> putStrLn $ processEncode args
        ("decode":args) -> putStrLn $ processDecode args
        _ -> putStrLn "I cannot do that"   
    if input /= "quit" then main else return ()  
          

processEncode :: [String] -> String
processEncode args = case args of
    (shiftStr:rest) ->
        case readMaybe shiftStr of
            Just shift -> unwords $ map (encode shift) (words $ unwords rest)
            Nothing -> "I cannot do that"
    _ -> "I cannot do that"

processDecode :: [String] -> String
processDecode args = case args of
    (shiftStr:rest) ->
        case readMaybe shiftStr of
            Just shift -> unwords $ map (decode shift) (words $ unwords rest)
            Nothing -> "I cannot do that"
    _ -> "I cannot do that"

encode :: Int -> String -> String
encode shift msg = map (charmap Data.Map.!) msg
  where charlist = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
        listlength = length charlist
        shiftedlist = take listlength (drop (shift `mod` listlength) (cycle charlist))
        charmap = Data.Map.fromList $ zip charlist shiftedlist

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing