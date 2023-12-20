-- func1 AH autumn 2023
import Dates
import Data.List (sortBy)

data E = E {
    e :: String,
    p :: String,
    d :: Date
} deriving (Show, Eq)

main :: IO ()
main = mainLoop []

mainLoop :: [E] -> IO ()
mainLoop initialEvents = do
    -- mapM_ (putStrLn . show) initialEvents
    input <- getLine
    putStrLn $ "> " ++ input
    let split = splitInput . trim $ input
    -- putStrLn $ unlines split
    let (response, updatedEvents) = validateInput split initialEvents
    putStrLn response
    if input /= "Quit" then mainLoop updatedEvents else return ()

-- e: 1, p: 4, d: 6
handleEvent :: [String] -> [E] -> (String, [E])
handleEvent ll ee =
    case parseDate (ll !! 6) of
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
    let mE = filter (\event -> e event == ll !! 3) ee
    in if null mE
        then ("I do not know of such event", ee)
        else (foldr (\event acc -> fE2 event ++ acc) "" mE, ee)

fE2 :: E -> String
fE2 ee = "Event " ++ e ee ++ " happens at " ++ p ee ++ " on " ++ show (d ee)

handleOn :: [String] -> [E] -> (String, [E])
handleOn ll ee =
    case parseDate (last ll) of
        Just date ->
            let se = sortBy cmp ee
                mE = filter (\event -> d event == date) se
                eventDescs = map fE3 mE
            in if null mE
                then ("Nothing that I know of", se)
                else (intercalate eventDescs, se)
        Nothing -> ("Bad date", ee)

fE3 :: E -> String
fE3 ee = "Event " ++ e ee ++ " happens on " ++ show (d ee)

-- Event Event G1 happens at Place G
handleAt :: [String] -> [E] -> (String, [E])
handleAt ll ee = 
    let se = sortBy cmp ee
        mE = filter (\event -> p event == ll !! 3) se
        eventDescs = map fE1 mE
    in if null mE
        then ("Nothing that I know of", se)
        else (intercalate eventDescs, se)

fE1 :: E -> String
fE1 ee = "Event " ++ e ee ++ " happens at " ++ p ee

validateInput :: [String] -> [E]-> (String, [E])
validateInput list events
    | length list == 1 && head list == "Quit"
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

cmp :: E -> E -> Ordering
cmp a b = compare (e a) (e b)

intercalate :: [String] -> String
intercalate [] = ""
intercalate [x] = x
intercalate (x:xs) = x ++ "\n" ++ intercalate xs

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\n' || c == '\t'

parseDate :: String -> Maybe Date
parseDate str = case map read $ wordsWhen (=='-') str of
    [y, m, d] ->
        if correctDate y m d
        then Just $ makeDate y m d
        else Nothing
    _ -> Nothing

splitInput :: String -> [String]
splitInput input = go input False ""
  where
    go [] _ acc = [acc]
    go (c:cs) inQuotes acc
        | c == '\'' = go cs (not inQuotes) acc
        | isSpace c && not inQuotes = 
            if null acc then go cs inQuotes "" 
            else acc : go cs inQuotes ""
        | otherwise = go cs inQuotes (acc ++ [c])

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'