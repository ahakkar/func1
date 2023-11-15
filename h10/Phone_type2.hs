-- func1 AH autumn 2023
module Phone_type2
  ( Phone(..),
    PhoneType(..),
    CountryCode(..),
    PhoneNo(..),
    fromPhoneNo,
    toPhoneNo,
    readPhone,
    studentReadPhone
  ) where

-- fromPhoneNo :: PhoneNo -> Integer
-- toPhoneNo :: Integer -> PhoneNo
-- readPhone :: String -> String -> String -> [Integer] -> Phone

-- | This should not be neccessary.
-- Should use import Text.Read(readMaybe) instead...
myReadMaybe :: Read a => String -> Maybe a
myReadMaybe s =
     case reads s of
        [(val, "")] -> Just val
        _           -> Nothing

-- | This should not be neccessary.
-- Should use import Data.List(isPrefixOf) instead :-(
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (x:xs) (y:ys) = x == y && myIsPrefixOf xs ys

-- | The 'Phone' type represents a phone number.
-- It has three fields: 'PhoneType', 'CountryCode', and 'PhoneNo'.
data Phone = Phone
    { phoneType :: PhoneType
    , countryCode :: CountryCode
    , phoneNo :: PhoneNo
    }
    deriving (Show, Eq)

-- | The 'PhoneType' type represents various kinds of phone numbers.
-- Each constructor indicates a different kind of phone.
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)


newtype CountryCode = CountryCode Integer deriving (Show, Eq)

newtype PhoneNo = PhoneNo Integer deriving (Show, Eq)



fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo n) = fromIntegral n


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

type PhoneTypeStr = String
type CountryCodeStr = String
type PhoneNoStr = String
type AllowedCountryCodes = [Integer]
-- | Constructs a 'Phone' object
-- 
-- @phonetypestr: The phone type as a String
-- @countrycodestr: The country code as a String
-- @phonenostr: The phone number as a String
-- @ccodelist: A list of valid country codes as Integers
--
-- Returns an 'Either' with error message ('String') or a valid 'Phone' object.
-- Example:
--
-- > studentReadPhone "WorkLandline" "+358" "1234567890" [358]
studentReadPhone :: 
    PhoneTypeStr -> 
    CountryCodeStr ->
    PhoneNoStr -> 
    AllowedCountryCodes -> 
    Either String Phone

studentReadPhone phonetypestr countrycodestr phonenostr ccodelist = do
    pType <- validatePhoneType phonetypestr
    cCode <- validateCountryCode countrycodestr ccodelist
    pNo <- validatePhoneNo phonenostr
    return $ Phone pType (toCountryCode cCode) (toPhoneNo pNo)


-- | Wrapper for 'studentReadPhone' that throws an error if the input is invalid.
readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone ptype ccode pno cclist = 
    case studentReadPhone ptype ccode pno cclist of
        Right phone -> phone
        Left err -> error err