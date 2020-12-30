{-# LANGUAGE FlexibleInstances #-}
-- Exercise 1
-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type
-- fib :: Integer -> Integer
-- so that fib n computes the nth Fibonacci number Fn.
-- Now use fib to define the infinite list of all Fibonacci numbers,
-- fibs1 :: [Integer]
-- (Hint: You can write the list of all positive integers as [0..].)
-- Try evaluating fibs1 at the ghci prompt. You will probably get
-- bored watching it after the first 30 or so Fibonacci numbers, because
-- fib is ridiculously slow. Although it is a good way to define the Fibonacci numbers, it is not a very good way to compute them—in order
-- to compute Fn it essentially ends up adding 1 to itself Fn times! For
-- example, shown at right is the tree of recursive calls made by evaluating fib 5.

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fib1 = map fib [0..]

-- Exercise 2
-- When I said “we” in the previous sentence I actually meant “you”.
-- Your task for this exercise is to come up with more efficient implementation. Specifically, define the infinite list
-- fibs2 :: [Integer]
-- so that it has the same elements as fibs1, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from the Prelude as appropriate.
fib' :: Integer -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = head $ foldr (\_ (y1:y2:ys) -> (y1 + y2):y1:[]) [1,0] [2..n]

fib2 = map fib' [0..]


-- Exercise 3
-- • Define a data type of polymorphic streams, Stream.
-- • Write a function to convert a Stream to an infinite list,
-- streamToList :: Stream a -> [a]
-- • To test your Stream functions in the succeeding exercises, it will be
-- useful to have an instance of Show for Streams. However, if you put
-- deriving Show after your definition of Stream, as one usually does,
-- the resulting instance will try to print an entire Stream—which,
-- of course, will never finish. Instead, you should make your own
-- instance of Show for Stream,
-- cis 194: homework 6 3
-- instance Show a => Show (Stream a) where
-- show ...
-- which works by showing only some prefix of a stream (say, the
-- first 20 elements).

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:(streamToList xs)

instance Show a => Show (Stream a) where
  show xs = (show (take 20 (streamToList xs)))

constructStream (x:xs) = Cons x (constructStream xs)
tailStream (Cons x xs) = xs
-- Exercise 4
-- Let’s create some simple tools for working with Streams.
-- • Write a function
-- streamRepeat :: a -> Stream a
-- which generates a stream containing infinitely many copies of the
-- given element.
-- • Write a function
-- streamMap :: (a -> b) -> Stream a -> Stream b
-- which applies a function to every element of a Stream.
-- • Write a function
-- streamFromSeed :: (a -> a) -> a -> Stream a
-- which generates a Stream from a “seed” of type a, which is the
-- first element of the stream, and an “unfolding rule” of type a -> a
-- which specifies how to transform the seed into a new seed, to be
-- used for generating the rest of the stream
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

fib'' = streamFromSeed (\(x,y) -> (x+y,x)) (0,1)

-- Exercise 5
-- Now that we have some tools for working with streams, let’s create a few:
-- • Define the stream
-- nats :: Stream Integer
-- which contains the infinite list of natural numbers 0, 1, 2, . . .
nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0
-- • Define the stream
-- ruler :: Stream Integer
-- which corresponds to the ruler function
-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
-- where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n.
powerOf2 :: Stream (Integer,Integer)
powerOf2 = streamFromSeed (\(x,y) -> (2 * x,y + 1)) (2,1)

highestOf2 :: Integer -> Integer
highestOf2 n
  | mod n 2 /= 0 = 0
  | otherwise = 1 + highestOf2 (div n 2)

ruler :: Stream Integer
ruler = streamMap highestOf2 (tailStream nats)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

streamIterate :: (Stream a -> Stream a -> Stream a) -> Stream (Stream a) -> Stream a
streamIterate f (Cons x xs) = f x (streamIterate f xs)
-- streamIterate (Cons x xs) = interleaveStreams x (streamIterate xs)

ruler' :: Stream Integer
ruler' = streamIterate interleaveStreams (streamMap streamRepeat (constructStream [0..]))

-- Exercise 6 (Optional)
-- • First, define
-- x :: Stream Integer
-- by noting that x = 0 + 1x + 0x
-- 2 + 0x
-- 3 + . . . .
-- • Define an instance of the Num type class for Stream Integer. Note that you will have to add
-- {-# LANGUAGE FlexibleInstances #-}
-- to the top of your .hs file in order for
-- this instance to be allowed.
-- Here’s what should go in your Num instance:
-- – You should implement the fromInteger function. Note that
-- n = n + 0x + 0x
-- 2 + 0x
-- 3 + . . . .
-- – You should implement negate: to negate a generating function,
-- negate all its coefficients.
-- – You should implement (+), which works like you would expect:
-- (a0 + a1x + a2x
-- 2 + . . .) + (b0 + b1x + b2x
-- 2 + . . .) = (a0 + b0) +
-- (a1 + b1)x + (a2 + b2)x
-- 2 + . . .
-- – Multiplication is a bit trickier. Suppose A = a0 + xA0 and
-- B = b0 + xB0 are two generating functions we wish to multiply.
-- We reason as follows:
-- AB = (a0 + xA0
-- )B
-- = a0B + xA0B
-- = a0(b0 + xB0
-- ) + xA0B
-- = a0b0 + x(a0B
-- 0 + A
-- 0B)
-- That is, the first element of the product AB is the product of
-- the first elements, a0b0; the remainder of the coefficient stream
-- (the part after the x) is formed by multiplying every element in
-- B
-- 0
-- (that is, the tail of B) by a0, and to this adding the result of
-- multiplying A
-- 0
-- (the tail of A) by B.
-- • The penultimate step is to implement an instance of the Fractional
-- class for Stream Integer. Here the important method to define is
-- division, (/). I won’t bother deriving it (though it isn’t hard), but
-- it turns out that if A = a0 + xA0 and B = b0 + xB0
-- , then A/B = Q,
-- where Q is defined as
-- Q = (a0/b0) + x((1/b0)(A
-- 0 − QB0
-- )).
-- That is, the first element of the result is a0/b0; the remainder is
-- formed by computing A
-- 0 − QB0 and dividing each of its elements
-- by b0.
-- Of course, in general, this operation might not result in a stream
-- of Integers. However, we will only be using this instance in cases
-- where it does, so just use the div operation where appropriate.

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons x xs) = Cons (-x) (negate xs)
  (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs + ys)
  (*) (Cons x xs) ys'@(Cons y ys) = Cons (x*y) ((fromInteger x) * ys + xs * ys')

instance Fractional (Stream Integer) where
  -- fromRational n = Cons (toInteger n) (streamRepeat 0)
  (/) (Cons _ xs) (Cons 0 ys) = xs / ys
  (/) xs'@(Cons x xs) ys'@(Cons y ys) = Cons (div x y) (streamMap (\n -> div n y) (xs - (xs'/ys') * ys))


fib3 :: Stream Integer
fib3 = (x) / (1 - x - x^2)


-- Exercise 7 (Optional)
-- • Create a type Matrix which represents 2 × 2 matrices of Integers.
-- • Make an instance of the Num type class for Matrix. In fact, you only
-- have to implement the (*) method, since that is the only one we
-- will use. (If you want to play around with matrix operations a bit
-- more, you can implement fromInteger, negate, and (+) as well.) Don’t worry about the warnings telling
-- you that you have not implemented the
-- other methods. (If you want to disable
-- the warnings you can add
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}
-- to the top of your file.)
-- • We now get fast (logarithmic time) matrix exponentiation for free,
-- since (^) is implemented using a binary exponentiation algorithm
-- in terms of (*). Write a function
-- fib4 :: Integer -> Integer
-- which computes the nth Fibonacci number by raising F to the nth
-- power and projecting out Fn (you will also need a special case
-- for zero). Try computing the one millionth or even ten millionth
-- Fibonacci number.
data Matrix = Mat Integer Integer Integer Integer deriving (Eq,Show)

instance Num Matrix where
  fromInteger n = Mat n 0 0 n
  (+) (Mat a11 a12 a21 a22) (Mat b11 b12 b21 b22) = Mat (a11+b11) (a12+b12) (a21+b21) (a22+b22)
  (*) (Mat a11 a12 a21 a22) (Mat b11 b12 b21 b22) = Mat c11 c12 c21 c22 where
                                                   c11 = a11*b11 + a12*b21
                                                   c12 = a11*b12 + a12*b22
                                                   c21 = a21*b11 + a22*b21
                                                   c22 = a21*b21 + a22*b22
  negate mat = 0 - mat

fetch :: Matrix -> Integer -> Integer -> Integer
fetch (Mat a11 a12 a21 a22) 1 1 = a11
fetch (Mat a11 a12 a21 a22) 1 2 = a12
fetch (Mat a11 a12 a21 a22) 2 1 = a21
fetch (Mat a11 a12 a21 a22) 2 2 = a22


fib4 :: Integer -> Integer
fib4 n = fetch f 1 1 where
  f = (Mat 1 1 1 0)^n
