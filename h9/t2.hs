-- func1 AH autumn 2023

-- | This should not be neccessary.
-- Should use import Text.Read(readMaybe) instead...
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
myReadMaybe :: (Read a) => String -> Maybe a
myReadMaybe s =
    case reads s of
        [(val, "")] -> Just val
        _ -> Nothing

-- | This should not be neccessary.
-- Should use import Data.List(isPrefixOf) instead :-(
myIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (x : xs) (y : ys) = x == y && myIsPrefixOf xs ys



type AllowedCountryCodes = [Integer]
type PhoneTypeStr = String
type CountryCodeStr = String
type PhoneNoStr = String
newtype CountryCode = CountryCode Integer deriving (Eq)
instance Show CountryCode where
    show (CountryCode n) = '+' : show n
newtype PhoneNo = PhoneNo Integer deriving (Eq)
instance Show PhoneNo where
    show (PhoneNo n) = show n

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)
{-
For the Phone type, implement your own instance for Show and make it
“pretty-print” the information in this form:

<country code><space><phone number><space><phone type in parenthesis>

e.g. “+358 123456789 (WorkLandline)”
-}
data Phone = Phone
    { 
        phoneType :: Maybe PhoneType,
        countryCode :: Maybe CountryCode,
        phoneNo :: PhoneNo
    }
    deriving (Eq)
instance Show Phone where
    show (Phone Nothing Nothing pn) = show pn
    show (Phone Nothing (Just cc) pn) = show cc ++ " " ++ show pn
    show (Phone (Just pt) Nothing pn) = show pn ++ " (" ++ show pt ++ ")"
    show (Phone (Just pt) (Just cc) pn) = show cc ++ " " ++ show pn ++ " (" ++ show pt ++ ")"

data PhoneBookEntry = PhoneBookEntry
    { 
        name :: String,
        phone :: Phone
    }
    deriving (Eq, Show)

type PhoneBook = [PhoneBookEntry]



-- | Convert an 'Integer' to a 'CountryCode'.
-- Throws an error if the input is negative.
--
-- Example:
--
-- > toCountryCode 358  -- Output: CountryCode 358
-- > toCountryCode (-1)  -- Output: error "Negative country code"
toCountryCode :: Integer -> CountryCode
toCountryCode x
    | x < 0 = error "Negative country code"
    | otherwise = CountryCode (fromIntegral x)

-- | Convert an 'Integer' to a 'PhoneNo'.
-- Throws an error if the input is negative.
--
-- Example:
--
-- > toPhoneNo 1234567890  -- Output: PhoneNo 1234567890
-- > toPhoneNo (-1)  -- Output: error "Negative phone number"
toPhoneNo :: Integer -> PhoneNo
toPhoneNo x
    | x < 0 = error "Negative phone number"
    | otherwise = PhoneNo (fromIntegral x)

-- | Validates a 'PhoneType' based on a string input.
--
-- Checks if the input matches any of the known 'PhoneType' constructors.
-- Returns 'Either' an error message ('String') or a valid 'PhoneType'.
--
-- Example:
--
-- > validatePhoneType "WorkLandline"  -- Output: Right WorkLandline
-- > validatePhoneType "UnknownType"   -- Output: Left "Incorrect phone type"
validatePhoneType :: String -> Either String PhoneType
validatePhoneType str
    | null str = Left "Missing phone type"
    | otherwise =
        case myReadMaybe str of
            Nothing -> Left "Incorrect phone type"
            Just pt -> Right pt

-- | Validates a 'CountryCode' based on a string input and a list of allowed codes.
--
-- Strips leading '+' or '00' and checks if the code is in the allowed list.
-- Returns 'Either' an error message ('String') or a valid 'CountryCode'.
--
-- Example:
--
-- > validateCountryCode "+358" [358]  -- Output: Right (CountryCode 358)
-- > validateCountryCode "999"  [358]  -- Output: Left "Unknown country code"
validateCountryCode :: String -> [Integer] -> Either String Integer
validateCountryCode str ccodelist =
    let processedStr
            | "+" `myIsPrefixOf` str = tail str
            | "00" `myIsPrefixOf` str = drop 2 str
            | otherwise = str
     in if null processedStr
            then Left "Empty country code"
            else case myReadMaybe processedStr of
                Nothing -> Left "Incorrect country code"
                Just cc ->
                    if cc `elem` ccodelist
                        then Right cc
                        else Left "Unknown country code"

-- | Validates a 'PhoneNo' based on a string input.
--
-- Checks if the phone number is in a valid format (only integers, etc.).
-- Returns 'Either' an error message ('String') or a valid 'PhoneNo'.
--
-- Example:
--
-- > validatePhoneNo "1234567890"  -- Output: Right (PhoneNo 1234567890)
-- > validatePhoneNo "123-456"     -- Output: Left "Incorrect phone number"
validatePhoneNo :: String -> Either String Integer
validatePhoneNo str
    | null str = Left "Empty phone number"
    | otherwise =
        case myReadMaybe str of
            Nothing -> Left "Incorrect phone number"
            Just pn -> Right pn


fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo n) = fromIntegral n
    

readPhoneType :: String -> Maybe PhoneType
readPhoneType str =
    case validatePhoneType str of
        Left _ -> Nothing
        Right pt -> Just pt

readCountryCode :: String -> [Integer] -> Maybe CountryCode
readCountryCode str ccodelist =
    case validateCountryCode str ccodelist of
        Left _ -> Nothing
        Right cc -> Just (toCountryCode cc)

readPhoneNo :: String -> PhoneNo
readPhoneNo str =
    case validatePhoneNo str of
        Left _ -> error "Incorrect phone number"
        Right pn -> toPhoneNo pn

-- Make the readPhone function accept empty strings for phone type and country
-- code. If they are empty make them Nothing.
readPhone :: String -> String -> String -> AllowedCountryCodes -> Phone
readPhone phonetypeStr countryCodeStr phoneNumberStr validCountryCodes =
    Phone (readPhoneType phonetypeStr)
          (readCountryCode countryCodeStr validCountryCodes)
          (readPhoneNo phoneNumberStr)


addEntry :: String -> String -> String -> String -> AllowedCountryCodes -> PhoneBook -> PhoneBook
addEntry nameStr phonetypeStr ccodeStr phonenumStr validCountryCodes currentBook =
    let phone = readPhone phonetypeStr ccodeStr phonenumStr validCountryCodes
    in if isDuplicateEntry nameStr phone currentBook
        then currentBook
        else PhoneBookEntry nameStr phone : currentBook


-- Checks if an entry with the same name and number already exists in the phone book
isDuplicateEntry :: String -> Phone -> PhoneBook -> Bool
isDuplicateEntry nameStr newPhone currentbook =
    any (\entry -> name entry == nameStr && phoneNo (phone entry) == phoneNo newPhone) currentbook

-- | Finds all entries in the phone book that match the given name.
findEntries :: String -> PhoneBook -> PhoneBook
findEntries searchName = filter (\entry -> searchName == name entry)

-- | Produce an empty phonebook
emptyBook :: PhoneBook
emptyBook = []








