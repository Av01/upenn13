{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Hw7.Scrabble where

import Data.Char
-- (1 point)-A, E, I, O, U, L, N, S, T, R
-- (2 points)-D, G
-- (3 points)-B, C, M, P
-- (4 points)-F, H, V, W, Y
-- (5 points)-K
-- (8 points)- J, X
-- (10 points)-Q, Z

newtype Score = Score Int
                deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c
  | elem (toLower c) ['a','e','i','o','u','l','n','s','t','r'] = Score 1
  | elem (toLower c) ['d','g'] = Score 2
  | elem (toLower c) ['b','c','m','p'] = Score 3
  | elem (toLower c) ['f','h','v','w','y'] = Score 4
  | elem (toLower c) ['k'] = Score 5
  | elem (toLower c) ['j','x'] = Score 8
  | elem (toLower c) ['q','z'] = Score 10
  | otherwise = Score 0

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

scoreString :: String -> Score
scoreString str = foldr (<>) (Score 0) (map score str)
