{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HW1 where
import Data.List(sort)

--1.1 & 1.2
data Royalty = Jack | Queen | King
    deriving (Eq, Enum)
data Suit = Hearts | Diamonds | Clubs | Spades
    deriving (Eq, Enum)
data Card = Aces Suit
            | Numeric Integer Suit
            | Face Royalty Suit
    deriving (Eq)

--1.3
type Hand = [Card]
type Deck = [Card]

--1.4
instance Show Royalty where
    show :: Royalty -> String
    show Jack = "J"
    show Queen = "Q"
    show King = "K"

-- instance Show Suit where
--     show :: Suit -> String
--     show Hearts = "H"
--     show Diamonds = "D"
--     show Clubs = "C"
--     show Spades = "S"

instance Show Card where
    show :: Card -> String
    show (Aces suit) = "A" ++  show suit
    show (Numeric n suit) = show n ++ show suit
    show (Face royal suit) = show royal ++ show suit

-- Bonus Exercise 1.5
instance Show Suit where
    show :: Suit -> String
    show Hearts = "\x2661"
    show Diamonds = "\x2662"
    show Clubs = "\x2663"
    show Spades = "\x2660"

--1.6
royals :: [Royalty]
royals = [Jack ..]

suits :: [Suit]
suits= [Hearts ..]

numbers :: [Integer]
numbers = [2 ..10]

fullDeck :: Deck
fullDeck = [Aces s | s <- suits]++ [Numeric i s| i<- numbers, s<- suits] ++ [Face r s| r<- royals, s<- suits]

--2.1
cardValue :: Card -> Integer
cardValue (Aces s) = 11
cardValue (Numeric n s) = n
cardValue (Face r s) = 10

-- test_case :: Hand
-- test_case = [Numeric 10 Hearts, Aces Hearts, Face Queen Spades]

handValue :: Hand -> Integer
handValue [] = 0
handValue (c:h) = cardValue c + handValue h

--or

handValue2 :: Hand -> Integer
handValue2 xs = sum (map cardValue xs)

--3.1
data Indexed i a = Indexed i a
    deriving (Show)

--3.2
instance Ord i => Ord (Indexed i a) where
    compare (Indexed x _) (Indexed y _) = compare x y
instance Eq i => Eq (Indexed i a) where
    (Indexed x _) == (Indexed y _) = x == y

--3.3
indexList :: [i] -> [a] -> [Indexed i a]
indexList xs ys = [Indexed x y | x<-xs | y<-ys]
getValue (Indexed i a) = a
unindexList :: [Indexed i a] -> [a]
unindexList = map getValue
--3.4
shuffle :: [Int] -> [a] -> [a]
shuffle i a = unindexList (sort (indexList i a))


