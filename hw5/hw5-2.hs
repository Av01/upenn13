{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
import Hw5.StackVM
import Hw5.Parser
import Hw5.Calc


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
