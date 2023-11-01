-- func1 AH autumn 2023

{-

Now, instead of using type synonyms, define data types CountryCode and PhoneNo 
so that both of them have a value constructor that takes an integer. Derive
instances for Eq and Show for CountryCode and PhoneNo.
 -}

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

newtype CountryCode = CountryCode Int deriving (Show, Eq)
newtype PhoneNo = PhoneNo Int deriving (Show, Eq)

{-
Make a function for both of them (toCountryCode and toPhoneNo) that takes an
Integer and throws an error if the integer is negative otherwise it creates 
the value. If CountryCode is negative, the error should be “Negative country
code” and if PhoneNo is negative, the error should be “Negative phone number”
and you should follow these literally to pass the automated testing.

Use the deriving keyword to derive an instance for Eq and Show for PhoneType.
-}
toCountryCode :: Integer -> CountryCode
toCountryCode x
    | x < 0 = error "Negative country code"
    | otherwise = CountryCode (fromIntegral x)

toPhoneNo :: Integer -> PhoneNo
toPhoneNo x
    | x < 0 = error "Negative phone number"
    | otherwise = PhoneNo (fromIntegral x)

{-

Again, using the record syntax, define Phone type for phone numbers that ha
only one value constructor with fields

phoneType :: PhoneType,
countryCode :: CountryCode, (this time the type defined as above)
phoneNo :: PhoneNo (this time the type defined as above)

Derive instance for Eq and Show for the record.-}

data Phone = Phone {
    phoneType :: PhoneType, 
    countryCode :: CountryCode,
    phoneNo :: PhoneNo 
} deriving (Show, Eq)

