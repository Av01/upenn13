{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
import StackVM
import Parser
import Calc


instance Expr Program where
    lit x = [PushI x]
    mul x y = x ++ y ++ [Mul]
    add x y = x ++ y ++ [Add]


compile :: String -> Maybe Program
compile = parseExp lit add mul

run :: String -> Either String StackVal
run str = case (compile str) of
          Just p -> stackVM p
          Nothing -> stackVM []
