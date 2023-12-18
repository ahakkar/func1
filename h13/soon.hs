
main :: IO ()
main = do
    input <- getLine
    putStrLn $ "> " ++ input
    let split = splitOnSpace . trim $ input
    putStrLn $ validateInput split
    if input /= "quit" then main else return ()  

-- really only haskell forces to write this kind of monsters
validateInput :: [String] -> String
validateInput list
    | length list == 1 && head list == "quit"
        = "Bye"
    | length list == 7
        && list !! 0 == "Event"
        && not (null (list !! 1))
        && list !! 2 == "happens"
        && list !! 3 == "at"
        && not (null (list !! 4))
        && list !! 5 == "on"
        && not (null (list !! 6)) = "Event"
    | length list == 4
        && list !! 0 == "Tell"
        && list !! 1 == "me"
        && list !! 2 == "about"
        && not (null (list !! 3)) = "Tell"
    | length list == 4
        && list !! 0 == "What"
        && list !! 1 == "happens"
        && list !! 2 == "on"
        && not (null (list !! 3)) = "What on"
    | length list == 4
        && list !! 0 == "What"
        && list !! 1 == "happens"
        && list !! 2 == "at"
        && not (null (list !! 3)) = "What at"
    | otherwise 
        = "I do not understand that. I understand the following:\n*Event <name> happens at <place> on <date>\n*Tell me about <eventname>\n*What happens on <date>\n*What happens at <place>\n*Quit"

-- stuff that should really be included by defualt
-- why must i write this
isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\n' || c == '\t'

-- really why i must write this too
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- writing functions like this in 2023 is utterly ridiculous
splitOnSpace :: String -> [String]
splitOnSpace [] = []
splitOnSpace str = word : splitOnSpace rest
  where
    word = takeWhile (/= ' ') str
    rest = dropWhile (== ' ') . dropWhile (/= ' ') $ str