-- func1 AH autumn 2023
module Eq3 (Eq3((===))) where
-- Definition of typeclass Eq3 here
-- The instance definitions should also added here
import Bool3 (Bool3(..))

class Eq3 a where
    (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
    (===) x y
        | x == Unk3 || y == Unk3 = Unk3
        | x == y = True3
        | otherwise = False3

instance Eq3 a => Eq3 (Maybe a) where
    (===) Nothing _ = Unk3
    (===) _ Nothing = Unk3
    (===) (Just x) (Just y) = x === y
