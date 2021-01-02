{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Hw7.JoinList where
import Hw7.Sized
import Hw7.Scrabble
import Hw7.Buffer


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) jl1 jl2 = Append ((tag jl1) <> (tag jl2)) jl1 jl2


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0 = Nothing
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ n (Append m jl jr)
  | v >= n + 1 = indexJ n jl
  | otherwise  = indexJ (n - v) jr
  where
    (Size v) = size $ tag jl

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ n Empty = Empty
dropJ n (Single _ _) = Empty
dropJ n j@(Append m jl jr)
  | n_all <= n = Empty
  | n_l <= n = dropJ (n-n_l) jr
  | otherwise = (dropJ n jl) +++ jr
  where (Size n_all) = size m
        (Size n_l) = size $ tag jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl | n < 0 = Empty
takeJ 0 _ = Empty
takeJ n j@(Single _ _) = j
takeJ n j@(Append m jl jr)
  | n_all <= n = j
  | n_l <= n = jl +++ (takeJ (n-n_l) jr)
  | otherwise = (takeJ n jl)
  where (Size n_all) = size m
        (Size n_l) = size $ tag jl


scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

scoreAndSizeLine :: String -> JoinList (Score,Size) String
scoreAndSizeLine str = Single ((scoreString str),Size 1) str

generateTree :: String -> JoinList (Score,Size) String
generateTree str = listToTree (map (\x -> Single ((scoreString x),Size 1) x) (lines str))

listToTree :: [JoinList (Score,Size) String] -> JoinList (Score,Size) String
listToTree [] = Empty
listToTree [x] = x
listToTree xs = (listToTree (take k xs)) +++ (listToTree (drop k xs))
              where k = div (length xs) 2

instance Buffer (JoinList (Score,Size) String) where
  fromString [] = Empty
  fromString str = generateTree str

  toString (Append _ jl jr) = toString jl ++ toString jr
  toString (Single _ str) = str
  toString Empty = []

  line = indexJ

  replaceLine n str j = (takeJ n j) +++ (fromString str) +++ (dropJ (n+1) j)

  numLines = getSize.snd.tag

  value = getScore.fst.tag





test = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
test1 = Append (Score 1, Size 4) (Append (Score 1, Size 3) (Single (Score 1, Size 1) 'y') (Append (Score 1, Size 2) (Single (Score 1, Size 1) 'e') (Single (Score 1, Size 1) 'a'))) (Single (Score 1, Size 1) 'h')
testStr = (Append (Score 23,Size 2) (Single (Score 9,Size 1) "yay ") (Single (Score 14,Size 1) "haskell!"))

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
