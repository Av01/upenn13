-- {-# LANGUAGE TypeSynonymInstances #-}
import Hw8.Employee
import Data.Tree
import Data.List
-- Exercise 1


glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs f) = GL (x:xs) (f + (empFun x))


instance Semigroup GuestList where
  (<>) (GL as f) (GL bs f') = GL (as ++ bs) (f + f')

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl gl'
  | (compare gl gl') == GT = gl
  | otherwise = gl'


-- Exercise 2

-- treeFold :: Monoid b => (a -> b -> b) -> b -> Tree a -> b
-- treeFold f c t = f (rootLabel t) (foldr (<>) mempty (map (treeFold f c) (subForest t)))
--
-- instance Semigroup Fun where
--   (<>) = (+)
--
-- instance Monoid Fun where
--   mempty = 0
--
-- -- treeFold (\t b -> (empFun t) + b) 0 testCompany

treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f c t = f (rootLabel t) (map (treeFold f c) (subForest t))

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)]
                      -> (GuestList, GuestList)
nextLevel e gLs = (glCons e (foldr (<>) mempty (map snd gLs)), foldr (<>) mempty (map fst gLs))


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun).(treeFold nextLevel [])

-- Exercise 5

instance Show GuestList where
  show (GL es f) = "Total fun:" ++ (show f) ++ "\n" ++ (foldl' (\c e -> c ++ e ++ "\n") "" (extractAndSort es))
                      where extractAndSort :: [Employee] -> [Name]
                            extractAndSort = sort.(map empName)

main :: IO ()
main = do
  str <- readFile ".\\hw8\\company.txt"
  putStr ((show.maxFun.read) str)
