import Dates

data E = E {
    e :: String,
    p :: String,
    t :: Date
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

handleEvent :: [String] -> [E] -> (String, [E])
handleEvent list events = ("Event", events)

handleTell :: [String] -> [E] -> (String, [E])
handleTell list events = ("Tell", events)

handleOn :: [String] -> [E] -> (String, [E])
handleOn list events = ("What on", events)

-- Event Event G1 happens at Place G
handleAt :: [String] -> [E] -> (String, [E])
handleAt ll ee = 
    let mE = filter (\event -> p event == ll !! 3) ee
    in if null mE
        then ("Nothing that I know of", ee)
        else (foldr (\event acc -> fE event ++ "\n" ++ acc) "" mE, ee)

fE :: E -> String
fE ee = "Event " ++ e ee ++ " happens at " ++ p ee

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