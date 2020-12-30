import Data.List

--Exercise 1 We need to first find the digits of a number. Define the
--functions
--toDigits :: Integer -> [Integer]
--toDigitsRev :: Integer -> [Integer]
--toDigits should convert positive Integers to a list of digits. (For 0 or
--negative inputs, toDigits should return the empty list.) toDigitsRev
--should do the same, but with the digits reversed.
--Example: toDigits 1234 == [1,2,3,4]
--Example: toDigitsRev 1234 == [4,3,2,1]
--Example: toDigits 0 == []
--Example: toDigits (-17) == []
extract :: Integer -> Integer -> [Integer]
extract 0 _ = []
extract n q = (mod n q):(extract (div n q) q)

reversed :: [a] -> [a]
reversed [] = []
reversed (x:xs) = (reversed xs) ++ (x:[])

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = extract n 10


toDigits :: Integer -> [Integer]
toDigits n = reversed $ extract n 10



--Exercise 2 Once we have the digits in the proper order, we need to
--double every other one. Define a function
--doubleEveryOther :: [Integer] -> [Integer]
--Remember that doubleEveryOther should double every other number beginning from the right, that is, the second-to-last, fourth-to-last,
-- . . . numbers are doubled.
--Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
--Example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryLeft :: [Integer] -> [Integer]
doubleEveryLeft [] = []
doubleEveryLeft (x:[]) = x:[]
doubleEveryLeft (x:y:rs) = x:(2*y):(doubleEveryLeft rs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reversed.doubleEveryLeft.reversed

--Exercise 3 The output of doubleEveryOther has a mix of one-digit
--and two-digit numbers. Define the function
--sumDigits :: [Integer] -> Integer
--to calculate the sum of all digits.
--Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumAll :: [Integer] -> Integer
sumAll [] = 0
sumAll (x:xs) = x + (sumAll xs)

toDigitsAndAdd :: [Integer] -> [Integer]
toDigitsAndAdd = map (sumAll.toDigits)

sumDigits :: [Integer] -> Integer
sumDigits = sumAll.toDigitsAndAdd 


--Exercise 4 Define the function
--validate :: Integer -> Bool
--that indicates whether an Integer could be a valid credit card number. This will use all functions defined in the previous exercises.
--Example: validate 4012888888881881 = True
--Example: validate 4012888888881882 = False

validate :: Integer -> Bool
validate n = (mod (sumDigits $ doubleEveryOther $ toDigits n) 10) == 0



--Exercise 5 The Towers of Hanoi is a classic puzzle with a solution
--that can be described recursively. Disks of different sizes are stacked
--on three pegs; the goal is to get from a starting configuration with
--all disks stacked on the first peg to an ending configuration with all
--disks stacked on the last peg, as shown in Figure 1.
--
--Figure 1: The Towers of Hanoi
--The only rules are
-- you may only move one disk at a time, and
-- a larger disk may never be stacked on top of a smaller one.
--For example, as the first move all you can do is move the topmost,
--smallest disk onto a different peg, since only one disk may be moved
--at a time.
--Figure 2: A valid first move. From this point, it is illegal to move to the configuration shown in
--Figure 3, because you are not allowed to put the green disk on top of
--the smaller blue one.
--Figure 3: An illegal configuration.
--To move n discs (stacked in increasing size) from peg a to peg b
--using peg c as temporary storage,
--1. move n − 1 discs from a to c using b as temporary storage
--2. move the top disc from a to b
--3. move n − 1 discs from c to b using a as temporary storage.
--For this exercise, define a function hanoi with the following type:
--type Peg = String
--type Move = (Peg, Peg)
--hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
--Given the number of discs and names for the three pegs, hanoi
--should return a list of moves to be performed to move the stack of
--discs from the first peg to the second.
--Note that a type declaration, like type Peg = String above, makes
--a type synonym. In this case Peg is declared as a synonym for String,
--and the two names Peg and String can now be used interchangeably.
--Giving more descriptive names to types in this way can be used to
--give shorter names to complicated types, or (as here) simply to help
--with documentation.
--Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
type Peg = String
type Move = (Peg,Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n <= 0 = []
    | n == 1 = [(a,b)]
    | otherwise = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)


--Exercise 6 (Optional) What if there are four pegs instead of three?
--That is, the goal is still to move a stack of discs from the first peg to
--the last peg, without ever placing a larger disc on top of a smaller
--one, but now there are two extra pegs that can be used as “temporary” storage instead of only one. Write a function similar to hanoi
--which solves this problem in as few moves as possible.
--It should be possible to do it in far fewer moves than with three
--pegs. For example, with three pegs it takes 215 − 1 = 32767 moves
--to transfer 15 discs. With four pegs it can be done in 129 moves. (See
--Exercise 1.17 in Graham, Knuth, and Patashnik, Concrete Mathematics,
--second ed., Addison-Wesley, 1994.)
comparePair :: ([a],Int) -> ([a],Int) -> Ordering
comparePair (_,l1) (_,l2) = compare l1 l2


hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c d
   | n <= 0 = []
   | n == 1 = [(a,b)]
   | n == 2 = [(a,c),(a,b),(c,b)]
   | n == 3 = [(a,d),(a,c),(a,b),(c,a),(d,a)]
   | otherwise = fst (sortBy comparePair (map (\x -> (x,length x)) (map (helper n a b c d) [1..(n-1)])) !! 0)

helper n a b c d k = (hanoi' k a c b d) ++ (hanoi (n-k-1) a d b) ++ [(a,b)] ++ (hanoi (n-k-1) d b a) ++ (hanoi' k c b d a)


--   | mod n 2 /= 0 = (hanoi' (k+1) a c b d) ++ (hanoi (k-1) a d b) ++ [(a,b)] ++ (hanoi (k-1) d b a) ++ (hanoi' (k+1) c b d a)
--   | mod n 2 == 0 = (hanoi' (k+1) a c b d) ++ (hanoi k a d b) ++ [(a,b)] ++ (hanoi k d b a) ++ (hanoi' (k+1) c b d a)
--                    where k = div (n-1) 2
--c_n = 1 + c_n-1

--c'_n = 2c'_n-1 + 1 
