module DataTypes where

import Data.List
import Test.QuickCheck

-- how to write programs in Haskell, using the BUILT IN data types (esp lists)

-- defining data types ~> defining a class

-- enums in Haskell
-- algebraic DATA types
data PrimaryColor
    = Red
    | Green
    | Blue
  deriving (Show)
-- PrimaryColor is a type
-- Red/Blue/Green are data constructors


x = Blue
-- pattern matching!

swizzle :: PrimaryColor -> PrimaryColor
swizzle Red = Blue
swizzle Blue = Green
swizzle Green = Red

-- Dynamic
data Json
  = JsonInt Int
  | JsonString String
  | JsonBool Bool
  | JsonList [Json]
  | JsonObject [String] [Json]
  deriving (Show)

-- how do we get data out of json?
-- pattern matching

printJson :: Json -> String
printJson (JsonInt x) = show x
printJson (JsonString x) = show x
printJson (JsonBool x) = show x
printJson (JsonList xs) =
  "[" ++ intercalate ", " (map printJson xs) ++ "]"
printJson (JsonObject ks xs) = undefined

{-
headInt :: [Int] -> Int
headPoly :: [a] -> a
-}

-- Maybe
data Failable a
  = Error
  | Result a
  deriving (Show)

safeHead :: [a] -> Failable a
safeHead [] = Error
safeHead (x:xs) = Result x

{-
zeroHead :: [Int] -> Int
zeroHead xs = case safeHead xs of
    Error -> 0
    Result x -> x
-}


-- Lists: [] = empty list, (:) = cons (add to the beginning of list)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

safeOurHead :: List a -> Failable a
safeOurHead Nil = Error
safeOurHead (Cons x xs) = Result x

-- In general...
--
-- data AlgDataType typevar1 typevar2
--    = Constr1 Type1 Type2
--    | Constr2 Type3
--    | Constr3 Type4 Type5 Type6
--    | Constr4
--
-- (record syntax)
--
-- Pattern matching general...
--
-- foo (Constr1 True "blah")
--  ~> x = True
--     y = "blah"
--     p = Constr1 True "blah"
--
-- foo p@(Constr1 x y) = ...
-- foo (Constr2 z) = ...
-- foo (Constr3 x y z) = ...
-- foo Constr4 = ...
--
-- foo _ = ...  -- don't care, match everything
--
-- "Make illegal states unrepresentable"


-- Trees
data Tree = Leaf Int
          | Node Tree Tree
    deriving (Show, Eq)

{-
instance Show Tree where
    show (Leaf _) = ...
-}

sumTree :: Tree -> Int
sumTree (Leaf i) = i
sumTree (Node l r) = sumTree l + sumTree r

foldTree :: (Int -> Int -> Int) -> Tree -> Int
foldTree _ (Leaf x) = x
foldTree f (Node t1 t2) = f (foldTree f t1) (foldTree f t2)


-- Laziness
data DoublyLinked a =
    N (Maybe (DoublyLinked a))
      a
      (Maybe (DoublyLinked a))
  deriving (Show)


prop_revrev :: [Int] -> Bool
prop_revrev x = reverse x == x

-- Number theory quickcheck

prop_even_odd n = even n ==> odd (n + 1)
m `divides` n = n `mod` m == 0

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
isPrime 2 = True
isPrime n = not $ any (`divides` n) [2..n-1]

prop_mersenne_is_prime n = isPrime n ==> isPrime (2^n - 1)

collatz :: Integer -> Bool
collatz 1 = True
collatz n = collatz (f n)
  where f n | even n = n `div` 2
            | odd n  = 3*n + 1



