-- func1 AH autumn 2023
module Phone_book_map
( PhoneBook,     
  Name,     
  addEntry,            
  findEntries,        
  emptyBook            
) where

import Phone_type2 (Phone(..), readPhone)

import qualified Data.Map as Map
type Name = String
type PhoneBook = Map.Map Name [Phone]


addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry nameStr phonetype ccode phonenum ccodelist currentBook =
    let phone = readPhone phonetype ccode phonenum ccodelist
        updatePhones mbPhones =  case mbPhones of
            Just phones -> if phoneNo phone `elem` map phoneNo phones
                        then Just phones  
                        else Just (phone : phones)  
            Nothing -> Just [phone]  
    in Map.alter updatePhones nameStr currentBook


findEntries :: Name -> PhoneBook -> [Phone]
findEntries name = Map.findWithDefault [] name


emptyBook :: PhoneBook
emptyBook = Map.empty