-- func1 AH autumn 2023

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

-- Using the type keyword, define two type synonyms for Integer: CountryCode and PhoneNo.
type CountryCode = Integer
type PhoneNo = Integer

{-
Then, using the record syntax (named fields), define Phone 
type for phone numbers that has only one value constructor 
with fields for (please write the fields in this order!)

phoneType :: PhoneType,
countryCode :: CountryCode (just a type synonym for Integer)
phoneNo :: PhoneNo (just a type synonym for Integer
).

Use the deriving keyword to derive instances for Show and Eq 
for the Phone type.
-}

data Phone = Phone {
    phoneType :: PhoneType, 
    countryCode :: CountryCode,
    phoneNo :: PhoneNo 
} deriving (Show, Eq)

{-

Make a function makePhone :: PhoneType -> CountryCode  -> PhoneNo -> Phone that 
throws an error (using the error function) if CountryCode or PhoneNo is a negative
integer and otherwise creates a value of type Phone with the given values.

If CountryCode is negative, the error should be “Negative country code”, and if PhoneNo
is negative, the error should be “Negative phone number” and you should use these 
strings literally to pass the automated testing.

(Note: Using type synonyms, you can still make the mistake of using a value of 
type CountryCode as PhoneNo.)

-}
makePhone :: PhoneType -> CountryCode  -> PhoneNo -> Phone
makePhone phoneType countryCode phoneNo
    | countryCode < 0 = error "Negative country code"
    | phoneNo < 0 = error "Negative phone number"
    | otherwise = Phone phoneType countryCode phoneNo

