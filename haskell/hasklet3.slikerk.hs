module Hasklet3 where

import Prelude hiding (Enum(..), sum)


--
-- * Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
          deriving (Eq,Show)

-- | The number 0.
zero = Zero

-- | The number 1.
one = Succ zero

-- | The number 2.
two = Succ one

-- | The number 3.
three = Succ two


-- | The successor of a natural number.
--   
--   >>> succ zero
--   Succ Zero
--   
--   >>> succ two
--   Succ (Succ (Succ Zero))
--   
succ :: Nat -> Nat
succ = Succ


-- | The predecessor of a natural number.
--   
--   >>> pred zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--  
pred :: Nat -> Nat
pred (Succ x) = x


-- | True if the given value is zero.
--
--   >>> isZero zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero a | (a == Zero) = True
	 | otherwise = False


-- | Convert a natural number to an integer.
--
--   >>> toInt zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ a) = 1 + (toInt(pred(Succ a)))

-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add zero one == one
--   True
--
--   >>> add two zero == two
--   True
--
--   >>> add two three == add three two
--   True
--   
-- This is the toNat helper function
toNat :: Int -> Nat
toNat n | (n > 0) = Succ(toNat(n - 1))
        | (n <= 0) = Zero

add :: Nat -> Nat -> Nat
add x y = toNat(toInt(x) + toInt(y))


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub x y = toNat(toInt(x) - toInt(y))


-- | Is the left value bigger than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt x y = toInt(x) > toInt(y)
	

-- | Multiply two natural numbers.
--
--   >>> mult two zero
--   Zero
--
--   >>> mult zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult x y = toNat(toInt(x) * toInt(y))


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum [] = zero
sum [x] = x
sum (x:xs) = toNat(toInt(x) + toInt(sum xs))


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
nats :: [Nat]
nats = zero : map succ nats

odds :: [Nat]
odds = map toNat(map (1+) ((map (*2) (map (toInt) nats)))) 

