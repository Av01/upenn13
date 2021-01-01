module Hw5.Calc where
import Hw5.ExprT
import Hw5.Parser

-- Exercise 1
-- Write Version 1 of the calculator: an evaluator for ExprT, with the
-- signature
-- eval :: ExprT -> Integer
-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y


-- Exercise 2
-- The UI department has internalized the focus group data and is
-- ready to synergize with you. They have developed the front-facing
-- user-interface: a parser that handles the textual representation of the
-- selected language. They have sent you the module Parser.hs, which
-- exports parseExp, a parser for arithmetic expressions. If you pass
-- the constructors of ExprT to it as arguments, it will convert Strings
-- representing arithmetic expressions into values of type ExprT. For
-- example:
-- *Calc> parseExp Lit Add Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- *Calc> parseExp Lit Add Mul "2+3*4"
-- Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
-- *Calc> parseExp Lit Add Mul "2+3*"
-- Nothing
-- Leverage the assets of the UI team to implement the value-added
-- function
-- evalStr :: String -> Maybe Integer
-- which evaluates arithmetic expressions given as a String, producing Nothing for inputs which are not well-formed expressions, and
-- Just n for well-formed inputs that evaluate to n.

evalStr :: String -> Maybe Integer
evalStr str = case (parseExp Lit Add Mul str) of
              (Just expr) -> Just (eval expr)
              Nothing -> Nothing


-- Exercise 3
-- Good news! Early customer feedback indicates that people really
-- do love the interface! Unfortunately, there seems to be some disagreement over exactly how the calculator should go about its calculating
-- business. The problem the software department (i.e. you) has is that
-- while ExprT is nice, it is also rather inflexible, which makes catering
-- to diverse demographics a bit clumsy. You decide to abstract away
-- the properties of ExprT with a type class.
-- Create a type class called Expr with three methods called lit, add,
-- and mul which parallel the constructors of ExprT. Make an instance of
-- Expr for the ExprT type, in such a way that
-- cis 194: homework 5 3
-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- Think carefully about what types lit, add, and mul should have. It
-- may be helpful to consider the types of the ExprT constructors, which
-- you can find out by typing (for example)
-- *Calc> :t Lit
-- at the ghci prompt

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  mul x y = Mul x y
  add x y = Add x y

recify :: ExprT -> ExprT
recify = id



-- The point of our Expr type class is that we can now write down
-- arithmetic expressions once and have them interpreted in various
-- ways just by using them at various types.
-- cis 194: homework 5 4
-- Make instances of Expr for each of the following types:
-- • Integer — works like the original calculator
-- • Bool — every literal value less than or equal to 0 is interpreted as False, and all positive Integers
-- are interpreted as True; “addition” is logical or,
-- “multiplication” is logical and
-- • MinMax — “addition” is taken to be the max function, while
-- “multiplication” is the min function
-- • Mod7 — all values should be in the ranage 0 . . . 6, and
-- all arithmetic is done modulo 7; for example,
-- 5 + 3 = 1.
-- The last two variants work with Integers internally, but in order
-- to provide different instances, we wrap those Integers in newtype
-- wrappers. These are used just like the data constructors we’ve seen
-- before.
-- newtype MinMax = MinMax Integer deriving (Eq, Show)
-- newtype Mod7 = Mod7 Integer deriving (Eq, Show)
-- Once done, the following code should demonstrate our family of
-- calculators:
-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + 5"
-- testInteger = testExp :: Maybe Integer
-- testBool = testExp :: Maybe Bool
-- testMM = testExp :: Maybe MinMax
-- testSat = testExp :: Maybe Mod7
-- Try printing out each of those tests in ghci to see if things are
-- working. It’s great how easy it is for us to swap in new semantics for
-- the same syntactic expression!

data MinMax = MinMax Integer
              deriving (Eq,Show)
data Mod7 = Mod7 Integer
            deriving (Eq,Show)

instance Expr Integer where
  lit x = x
  mul x y = x * y
  add x y = x + y

instance Expr Bool where
  lit x = if x > 0 then True else False
  mul x y = x && y
  add x y = x || y

instance Expr MinMax where
  lit x = MinMax x
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (mul x y::Integer) 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (add x y::Integer) 7)

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul
