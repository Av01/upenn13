{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
import Parser
import Calc
import qualified Data.Map as M

data VarExpt = Lit Integer
             | Add VarExpt VarExpt
             | Mul VarExpt VarExpt
             | Var String
             deriving (Eq,Show)

class HasVars a where
  var :: String -> a

instance HasVars VarExpt where
  var str = Var str

instance Expr VarExpt where
  lit x = Lit x
  mul x y = Mul x y
  add x y = Add x y

perform :: (Integer -> Integer -> Integer)
            -> Maybe Integer
            -> Maybe Integer
            -> Maybe Integer
perform f Nothing _ = Nothing
perform f _ Nothing = Nothing
perform f (Just a) (Just b) = Just (f a b)

evalWithVars :: VarExpt -> M.Map String Integer -> Maybe Integer
evalWithVars (Lit x) _ = Just x
evalWithVars (Var str) vmap = M.lookup str vmap
evalWithVars (Mul x y) vmap = perform (*) (evalWithVars x vmap) (evalWithVars y vmap)
evalWithVars (Add x y) vmap = perform (+) (evalWithVars x vmap) (evalWithVars y vmap)

--now we are using type-classes to develop same functionally

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var str = (\m -> M.lookup str m)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\m -> Just x)
  mul x y = (\m -> perform (*) (x m) (y m) )
  add x y = (\m -> perform (+) (x m) (y m) )


withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer

withVars vs exp = exp $ M.fromList vs
