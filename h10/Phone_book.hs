-- func1 AH autumn 2023
module Phone_book
( PhoneBookEntry(..),  
  PhoneBook,          
  addEntry,            
  findEntries,        
  emptyBook            
) where

import Phone_type2 (Phone(..), studentReadPhone)

-- findEntries :: Name -> PhoneBook -> [Phone]
-- addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
-- emptyBook :: PhoneBook

-- | The 'PhoneBookEntry' type represents a phone book entry.
--   It has two fields: 'String' for the name and 'Phone' for the phone number.
data PhoneBookEntry = PhoneBookEntry
    { name :: String
    , phone :: Phone
    }
    deriving (Eq, Show)

type PhoneBook = [PhoneBookEntry]

addEntry ::
    String ->        -- | PhoneType as String            
    String ->        -- | CountryCode as String    
    String ->        -- | PhoneNo as String   
    String ->        -- | List of valid country codes   
    [Integer] ->     -- | Current phone book   
    PhoneBook ->     -- | Updated phone book or error message   
    PhoneBook
addEntry nameStr phonetype ccode phonenum ccodelist currentbook =
    case studentReadPhone phonetype ccode phonenum ccodelist of
        Right phone ->
            if isDuplicateEntry nameStr phone currentbook
            then currentbook
            else PhoneBookEntry nameStr phone : currentbook
        Left _ -> currentbook

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

