-- Exercise 1: Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1’ and fun2’ respectively.
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)
-- Hint: For this problem you may wish to use the functions iterate
-- and takeWhile. Look them up in the Prelude documentation to see
-- what they do.
fun1' :: [Integer] -> Integer
fun1' = product.map ((-)2).filter even

fun2' :: Integer -> Integer
fun2' = sum.filter even.takeWhile (/=1).iterate step

step :: Integer -> Integer
step n
  | even n = n `div` 2
  | otherwise = 3 * n + 1


  -- Exercise 2: Folding with trees
  -- Recall the definition of a binary tree data structure. The height of http://en.wikipedia.org/wiki/
  -- Binary_tree a binary tree is the length of a path from the root to the deepest
  -- node. For example, the height of a tree with a single node is 0; the
  -- height of a tree with three nodes, whose root has two children, is 1;
  -- and so on. A binary tree is balanced if the height of its Leaf and right
  -- subtrees differ by no more than 1, and its Leaf and right subtrees are
  -- also balanced.
  -- You should use the following data structure to represent binary
  -- trees. Note that each node stores an extra Integer representing the
  -- height at that node.
  -- data Tree a = Leaf
  -- | Node Integer (Tree a) a (Tree a)
  -- deriving (Show, Eq)
  -- For this exercise, write a function
  -- cis 194: homework 4 2
  -- foldTree :: [a] -> Tree a
  -- foldTree = ...
  -- which generates a balanced binary tree from a list of values using
  -- foldr.
  -- For example, one sample output might be the following, also visualized at right:

  -- foldTree "ABCDEFGHIJ" ==
  -- Node 3 (Node 2 (Node 0 Leaf 'F' Leaf) 'I' (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf)) 'J' (Node 2 (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf) 'H' (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
  -- Your solution might not place the nodes in the same exact order,
  -- but it should result in balanced trees, with each subtree having a
  -- correct computed height.


data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
          deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

nodes :: Tree a -> Integer
nodes Leaf = 0
nodes (Node _ Leaf _ Leaf) = 1
nodes (Node _ lt _ rt) = 1 + (nodes lt) + (nodes rt)

balanced :: Tree a -> Bool
balanced Leaf = True
balanced (Node _ lt _ rt) = (nodes lt) == (nodes rt)

addNode :: a -> Tree a -> Tree a
addNode x Leaf = Node 0 Leaf x Leaf
addNode x (Node h lt@Leaf y rt@Leaf) = Node (h+1) (addNode x rt) y lt
addNode x (Node h lt@Leaf y rt) = Node h (addNode x lt) y rt
addNode x (Node h lt y rt@Leaf) = Node h lt y (addNode x rt)
addNode x t@(Node h lt y rt)
    | balanced t = Node (h+1) (addNode x lt) y rt
    | otherwise = if balanced lt
                  then Node h lt y (addNode x rt)
                  else Node h (addNode x lt) y rt

-- 1. Implement a function
-- xor :: [Bool] -> Bool
-- which returns True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains. For example,
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
-- Your solution must be implemented using a fold.
xor :: [Bool] -> Bool
xor = odd.sum.foldr (\x y-> if x then 1:y else 0:y) []

-- 2. Implement map as a fold. That is, complete the definition
-- map’ :: (a -> b) -> [a] -> [b]
-- map’ f = foldr ...
-- in such a way that map’ behaves identically to the standard map
-- function.
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ys -> (f x):ys) [] xs


-- 3. (Optional) Implement foldl using foldr. That is, complete the
-- definition
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr ...
-- in such a way that myFoldl behaves identically to the standard
-- foldl function.
-- Hint: Study how the application of foldr and foldl work out:
-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g b -> g (f b x) ) id xs base


--
-- Exercise 4: Finding primes
-- Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
-- of_Sundaram ing function composition. Given an integer n, your function should
-- generate all the odd prime numbers up to 2n + 2.
-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram = ...


-- To give you some help, below is a function to compute the Cartesian product of two lists. This is similar to zip, but it produces all
-- possible pairs instead of matching up the list elements. For example,
-- cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
-- It’s written using a list comprehension, which we haven’t talked about
-- in class (but feel free to research them).
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram 0 = [0]
sieveSundaram 1 = [0]
sieveSundaram 2 = [2]
sieveSundaram n = map (\x->2*x + 1) (filter (\x -> not $ elem x gs) [1..n])
      where gs = generatePairs n

generatePairs :: Integer -> [Integer]
generatePairs n = filter (<=n) $ map (\(x,y) -> x + y + 2 * x * y) (cartProd [1..n] [1..n])
