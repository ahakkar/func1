-- func1 AH autumn 2023
module Phone_book_tree
( PhoneBook(..),     
  Name,     
  addEntry,            
  findEntries,        
  emptyBook            
) where

import Phone_type2 (Phone(..), readPhone)

-- Exercise-provided BST structure
data PhoneBook = Empty | Node Name [Phone] PhoneBook PhoneBook deriving (Show, Eq)
type Name = String


addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry nameStr phonetype ccode phonenum ccodelist currentBook =
    let phone = readPhone phonetype ccode phonenum ccodelist
    in insert nameStr phone currentBook



insert :: Name -> Phone -> PhoneBook -> PhoneBook
insert name phone Empty = Node name [phone] Empty Empty
insert name phone (Node a phones left right)
    | name == a = if phoneNo phone `elem` map phoneNo phones
                then Node a phones left right
                else Node a (phone:phones) left right
    | name < a  = Node a phones (insert name phone left) right
    | otherwise = Node a phones left (insert name phone right)



findEntries :: Name -> PhoneBook -> [Phone]
findEntries name Empty = []
findEntries name (Node a phones left right)
    | name == a = phones
    | name < a  = findEntries name left
    | otherwise = findEntries name right


emptyBook :: PhoneBook
emptyBook = Empty
