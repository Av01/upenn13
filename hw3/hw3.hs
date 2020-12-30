--Exercise 1 Hopscotch
--Your first task is to write a function
--cis 194: homework 3 3
--skips :: [a] -> [[a]]
--The output of skips is a list of lists. The first list in the output should
--be the same as the input list. The second list in the output should
--contain every second element from the input list. . . and the nth list in
--the output should contain every nth element from the input list.
--For example:
--skips "ABCD" == ["ABCD", "BD", "C", "D"]
--skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
--skips [1] == [[1]]
--skips [True,False] == [[True,False], [False]]
--skips [] == []
--Note that the output should be the same length as the input.
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (filterByIndex xs_i) xs_i
    where xs_i = (zip xs [1..(length xs)])

filterByIndex :: [(a,Int)] -> (a,Int) -> [a]
filterByIndex xs (_,i) = map (\x -> fst x) (filter (selector i) xs)

selector :: Int -> (a,Int) -> Bool
selector i (_,k) = if i == 1
                   then True
                   else (mod k i) == 0

--Exercise 2 Local maxima
--A local maximum of a list is an element of the list which is strictly
--greater than both the elements immediately before and after it. For
--example, in the list [2,3,4,1,5], the only local maximum is 4, since
--it is greater than the elements immediately before and after it (3 and
--1). 5 is not a local maximum since there is no element that comes
--after it.
--Write a function
--localMaxima :: [Integer] -> [Integer]
--which finds all the local maxima in the input list and returns them in
--order. For example:
--localMaxima [2,9,5,6,1] == [9,6]
--localMaxima [2,3,4,1,5] == [4]
--localMaxima [1,2,3,4,5] == []


-- localMaxima :: [Integer] -> [Integer]
-- localMaxima (x1:x2:x3:xs) = (maxOfThree x1 x2 x3) ++ localMaxima (x2:x3:xs)
-- localMaxima _ = []
--

localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldr maxOfThree [] (zip3 xs (drop 1 xs) (drop 2 xs))

maxOfThree :: (Integer,Integer,Integer) -> [Integer] -> [Integer]
maxOfThree (i,j,k) ys
    | i < j && j > k = j:ys
    | otherwise = ys
--
-- -- someFunc :: [Integer] -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> [Integer]
-- someFunc rs (Just x1) (Just x2) (Just x3) = (\z -> someFunc ((maxOfThree x1 x2 x3) ++ rs) (Just x2) (Just x3) z)
-- someFunc rs _ _ Nothing = (\z -> rs)
-- someFunc rs Nothing x1 x2 = (\z -> someFunc rs x1 x2 z)
-- someFunc rs Nothing Nothing x1 = (\z -> someFunc rs Nothing x1 z)
--
--
-- localMaxima :: [Integer] -> [Integer]
-- localMaxima xs = (foldr (\x y -> y (Just x)) (someFunc [] Nothing Nothing) xs) Nothing Nothing

-- localMaxima :: [Integer] -> [Integer]
-- localMaxima xs = (foldr (\x f e -> (\x' -> (f x):e)) (someFunc Nothing Nothing) xs) Nothing
-- f = \e1 e2 -> fi x1 e1 e2
-- f = \e1 e2 ->

-- someFunc :: [Integer] -> Maybe Integer -> Maybe Integer -> Maybe Integer -> [Integer]
-- someFunc rs _ _ Nothing = (\x -> rs)
-- someFunc rs Nothing x1 x2 = someFunc rs x1 x2
-- someFunc rs Nothing Nothing x1 = someFunc rs Nothing x1
-- someFunc rs (Just x1) (Just x2) (Just x3) = someFunc ((maxOfThree x1 x2 x3) ++ rs) (Just x2) (Just x3)


-- [2,4,3,5]
-- someFunc [] Nothing Nothing 5
-- (\z -> someFunc [] Nothing 5 z)
-- someFunc [] Nothing 5 3
-- (\z -> someFunc [] 5 3 z)
-- someFunc [] 5 3 4
-- (\z -> someFunc (maxOfThree 5 3 4 ++ []) 3 4 z)
-- someFunc (maxOfThree 5 3 4 ++ []) 3 4 2
-- (\z -> someFunc (maxOfThree 3 4 2 ++ (maxOfThree 5 3 4 ++ [])) 4 2 z)
-- someFunc (maxOfThree 3 4 2 ++ (maxOfThree 5 3 4 ++ [])) 4 2 Nothing
-- (/z -> (maxOfThree 3 4 2 ++ (maxOfThree 5 3 4 ++ []))) Nothing


--Exercise 3 Histogram
--For this task, write a function
--histogram :: [Integer] -> String
--which takes as input a list of Integers between 0 and 9 (inclusive),
--and outputs a vertical histogram showing how many of each number
--were in the input list. You may assume that the input list does not
--contain any numbers less than zero or greater than 9 (that is, it does
--not matter what your function does if the input does contain such
--numbers). Your output must exactly match the output shown in the
--examples below.
--cis 194: homework 3 4
--histogram [1,1,1,5] ==
-- *
-- *
-- * *
-- ==========
--0123456789
--histogram [1,4,5,4,6,6,3,4,2,4,9] ==
-- *
-- *
-- * *
-- ****** *
-- ==========
--0123456789
--Important note: If you type something like histogram [3,5] at
--the ghci prompt, you should see something like this:
--" * * \n==========\n0123456789\n"
--This is a textual representation of the String output, including \n
--escape sequences to indicate newline characters. To actually visualize
--the histogram as in the examples above, use putStr, for example,
--putStr (histogram [3,5]).

histogram :: [Integer] -> String
histogram = reduceToString.combineHorzontally.equalize.reduceToStars

reduceToString :: [String] -> String
reduceToString xs = foldr (\x y -> x ++ "\n" ++ y) "" xs

combineHorzontally :: [String] -> [String]
combineHorzontally xs = foldr combine (take (length xs) $ repeat []) xs

combine :: String -> [String] -> [String]
combine xs ys = map (\(x,y) -> x:y) (zip xs ys)


addSpaces :: Int -> String -> String
addSpaces k x = (take (k - length x) $ repeat ' ') ++ x

equalize :: [String] -> [String]
equalize xs = map (addSpaces k) xs
  where
    k = maxList (map length xs)

maxList :: [Int] -> Int
maxList = foldr (\x y -> if x > y then x else y) 0

reduceToStars :: [Integer] -> [String]
reduceToStars xs = map fst (foldr increaseStar (zip ["=0","=1","=2","=3","=4","=5","=6","=7","=8","=9"] [0..9]) xs)

increaseStar :: Integer -> [(String,Integer)] -> [(String,Integer)]
increaseStar i xs = map (\(x,y) -> if y == i then (('*':x),y) else (x,y)) xs
