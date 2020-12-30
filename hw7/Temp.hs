-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Sum a = Sum a
  deriving (Eq, Ord, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Num (Sum a) where
  (+) (Sum x) (Sum y) = Sum (x + y)
  negate (Sum x) = Sum (-x)

instance Num a => Semigroup (Sum a) where
  (<>) = (+)

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
