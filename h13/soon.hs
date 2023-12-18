import Dates

data E = E {
    e :: String,
    p :: String,
    d :: Date
} deriving (Show, Eq)

main :: IO ()
main = mainLoop []

mainLoop :: [E] -> IO ()
mainLoop initialEvents = do
    input <- getLine
    putStrLn $ "> " ++ input
    let split = splitOnSpace . trim $ input
    let (response, updatedEvents) = validateInput split initialEvents
    putStrLn response
    if input /= "quit" then mainLoop updatedEvents else return ()

-- e: 1, p: 4, d: 6
handleEvent :: [String] -> [E] -> (String, [E])
handleEvent ll ee =
    case parseDate (last ll) of
        Just date ->
            let nE = E { e = ll !! 1, p = ll !! 4, d = date }
                uE = aOUE nE ee
            in ("Ok", uE)
        Nothing -> ("Bad date", ee)

fdE :: (E -> Bool) -> [E] -> Maybe E
fdE _ [] = Nothing
fdE predicate (x:xs) =
    if predicate x
    then Just x
    else fdE predicate xs

aOUE :: E -> [E] -> [E]
aOUE nE ee =
    let eeE = fdE (\event -> e event == e nE) ee
    in case eeE of
        Just _ -> map (\event -> if e event == e nE then nE else event) ee
        Nothing -> nE : ee

handleTell :: [String] -> [E] -> (String, [E])
handleTell ll ee = 
    let mE = filter (\event -> p event == ll !! 3) ee
    in if null mE
        then ("I do not know of such event", ee)
        else (foldr (\event acc -> fE2 event ++ "\n" ++ acc) "" mE, ee)

fE2 :: E -> String
fE2 ee = "Event " ++ e ee ++ " happens at " ++ p ee ++ " on " ++ show (d ee)

handleOn :: [String] -> [E] -> (String, [E])
handleOn ll ee =
    case parseDate (last ll) of
        Just date ->
            let mE = filter (\event -> d event == date) ee
            in if null mE
                then ("Nothing that I know of", ee)
                else (foldr (\event acc -> fE3 event ++ "\n" ++ acc) "" mE, ee)
        Nothing -> ("Bad date", ee)

fE3 :: E -> String
fE3 ee = "Event " ++ e ee ++ " happens on " ++ show (d ee)

-- Event Event G1 happens at Place G
handleAt :: [String] -> [E] -> (String, [E])
handleAt ll ee = 
    let mE = filter (\event -> p event == ll !! 3) ee
    in if null mE
        then ("Nothing that I know of", ee)
        else (foldr (\event acc -> fE1 event ++ "\n" ++ acc) "" mE, ee)

fE1 :: E -> String
fE1 ee = "Event " ++ e ee ++ " happens at " ++ p ee

-- really only haskell forces to write this kind of monsters
validateInput :: [String] -> [E]-> (String, [E])
validateInput list events
    | length list == 1 && head list == "quit"
        = ("Bye", events)
    | length list == 7
        && list !! 0 == "Event"
        && not (null (list !! 1))
        && list !! 2 == "happens"
        && list !! 3 == "at"
        && not (null (list !! 4))
        && list !! 5 == "on"
        && not (null (list !! 6)) = handleEvent list events
    | length list == 4
        && list !! 0 == "Tell"
        && list !! 1 == "me"
        && list !! 2 == "about"
        && not (null (list !! 3)) = handleTell list events
    | length list == 4
        && list !! 0 == "What"
        && list !! 1 == "happens"
        && list !! 2 == "on"
        && not (null (list !! 3)) = handleOn list events
    | length list == 4
        && list !! 0 == "What"
        && list !! 1 == "happens"
        && list !! 2 == "at"
        && not (null (list !! 3)) = handleAt list events
    | otherwise 
        = ("I do not understand that. I understand the following:\n*Event <name> happens at <place> on <date>\n*Tell me about <eventname>\n*What happens on <date>\n*What happens at <place>\n*Quit", events)

parseDate :: String -> Maybe Date
parseDate str = case map read $ wordsWhen (=='-') str of
    [y, m, d] -> Just $ makeDate y m d
    _ -> Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

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