{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}
module Lambda where

import Test.QuickCheck

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char
import Data.String
import Data.List

-- In this lab, we will implement a small-step evaluator for the lambda
-- calculus using substitution.  While the "stated" goal of this lab is
-- to give you some experience implementing substitution (an important
-- ingredient for any real world compiler of a functional programming
-- language), there is also a hidden goal giving you an example of
-- "threading state" which will be helpful motivation for monads,
-- which we will look at later in the course.
--
-- You may find it convenient to call GHCi with the following flag:
--
--      ghci -XOverloadedStrings Lambda.hs
--
-- This enables a syntax-extension, overloaded strings, which will
-- allow you to type Haskell-style "\\x -> x" for a lambda expression,
-- instead of having to type out Lambda "x" (Var "x").  (Note
-- that if you are typing a String literal, backslashes must be
-- escaped.)
--
-- We must first start by defining a data type for our lambda calculus.
-- It is a very simple data type which directly reflects the grammar
-- for the lambda calculus.  We represent names as simple Haskell
-- strings.

data Expr = Var Name
          | Lambda Name Expr
          | App Expr Expr
    deriving (Eq)

newtype Name = Name String
    deriving (Eq, Ord)

-- Write a function "free variables" which computes a set of
-- the free variables of a lambda term.  Sets can be manipulated
-- using the following functions, which "do the obvious thing".
--
--      Set.empty     :: Set a
--      Set.singleton :: a -> Set a
--      Set.delete    :: a -> Set a -> Set a
--      Set.union     :: Set a -> Set a -> Set a
--      Set.unions    :: [Set a] -> Set a
--      Set.member    :: a -> Set a -> Bool
--
-- The full documentation for Set functions can be found here:
-- http://hackage.haskell.org/package/containers/docs/Data-Set.html
-- You may also find the Hoogle tool useful:
-- https://www.haskell.org/hoogle/
--
-- (Haskell tip: the period in these names are a qualifier, which help
-- us disambiguate functions which operate on Sets from functions which
-- operate on Maps, which we will use later.  Just pretend Set.delete is
-- a function like setDelete would be.)
--
-- (Haskell tip: Actually, these functions have slightly more
-- complicated type signatures, which you can check in GHCi.  The "Ord a"
-- is a constraint which says that "a" must be a *comparable* type.
-- We will talk more about this in the type classes lecture.)

fv :: Expr -> Set Name
-- BEGIN fv (DO NOT DELETE THIS LINE)
fv = undefined
-- END fv (DO NOT DELETE THIS LINE)

-- Here are a few unit tests for free variables:
--
--      fv "x"        == Set.fromList ["x"]
--      fv "\\x -> x" == Set.fromList []
--      fv "\\x -> y" == Set.fromList ["y"]
--      fv "x y"      == Set.fromList ["x", "y"]

-- A substitution is a mapping from a 'Name' to an 'Expr'.
-- As a visual cue for the expression which is part of a substitution
-- (which is distinct for the expression we are substituting over),
-- let us define a type synonym for substitutions:

type Subst = (Name, Expr)

-------------------------------------------------------------------

--                  Pure (inefficient) substitution

-------------------------------------------------------------------

-- In this section, we will implement substitution as a nearly
-- direct transcription from the rules we saw in lecture.

-- We've provided a function that, for some set S and a variable y
-- in S, finds a variable y' such that y /= y' and y' is not in S.
-- If S is the set of in-scope variables, this gives us a *fresh variable*.
--
-- Here are some sample invocations of the function.  Feel free to
-- try other invocations in GHCi
--
--  > fresh (Set.fromList []) "y"
--  "y1"
--  > fresh (Set.fromList ["y1"]) "y"
--  "y2"

fresh :: Set Name -- A set of forbidden variable names
      -> Name -- The previous name
      -> Name -- A new name not in the set, based off of previous name.
fresh in_scope (Name x) = Name (head (filter p cands))
    where
        p x' = x /= x' && Name x' `Set.notMember` in_scope
        -- There are better strategies for generating a list of
        -- candidate variables (this strategy could end up forcing
        -- us to try a lot of variables before we find a fresh one),
        -- but this will be adequate for this lab.
        cands = map ((takeWhile isAlpha x ++)) $ map show [(1 :: Int)..]

-- Implement capture-avoiding substitution on expressions.  This should
-- be a fairly direct transcription of the rules from lecture.
--
-- Here is a sample transcript with the reference implementation:
--
--  *Lambda> subst "x" ("x", "y")
--  "y"
--  *Lambda> subst "\\x -> x" ("x", "y")
--  "\\x -> x"
--  *Lambda> subst "\\x -> \\x -> x" ("x", "y")
--  "\\x -> \\x -> x"
--  *Lambda> subst "\\y -> \\z -> x" ("x", "y")
--  "\\y1 -> \\z -> y"
--  *Lambda> subst "(\\y -> x) (\\y -> x)" ("x", "y")
--  "(\\y1 -> y) (\\y1 -> y)"
--  *Lambda> subst "x y" ("x", "y")
--  "y y"
--  *Lambda> subst "\\x1 -> x x3" ("x", "x1 x2")
--  "\\x4 -> x1 x2 x3"
--
-- Your implementation does not have to produce exactly the same
-- bound variable names as above; the outputs simply have to be
-- alpha equivalent.  However, if you would like to get similar
-- output, consider what should happen (1) when the variable you are
-- substituting matches the binder of a lambda, and (2) if the variable
-- you are substituting is not in the free variables of the body of a
-- lambda.

subst :: Expr -> Subst -> Expr
-- BEGIN subst (DO NOT DELETE THIS LINE)
subst = undefined
-- END subst (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  Alpha equivalence

-------------------------------------------------------------------

-- We should write some tests for our substitution functions.  However,
-- most properties we'd like to test require alpha-equivalence.
-- Haskell's default equality will not work: it will claim that "\\x ->
-- x" is not equal to "\\y -> y".  In short, we will need to define
-- a function to compute alpha equivalence for us.
--
-- In this lab, we will implement alpha-equivalence by *converting*
-- our lambda terms into a "locally nameless" representation, for which
-- structural equality coincides with alpha equivalence.  The basic
-- idea is to replace bound variables names (which can be alpha
-- renamed) with 0-indexed *number*, which is unique.  For example,
-- instead of representing "\x -> x" as:
--
--      Lambda "x" (Var "x")
--
-- We instead represent it as:
--
--      LambdaD (BoundVarD 0)
--
-- Here, the "zero" means that the variable was bound by the immediately
-- enclosing lambda-binder.  Similarly:
--
--      Lambda "x" (Lambda "y" (Var "x"))
--          ==>
--      LambdaD (LambdaD (BoundVarD 1))
--          ^-----------------/
--
--
-- Here is the data type for locally nameless lambda terms.  The key
-- differences are that (1) Var has been split into two constructors
-- FreeVarD and BoundVarD, representing free and bound variables, and
-- (2) LambdaD implicitly binds variable (so it doesn't need a Name.)

data ExprD = FreeVarD Name
           | BoundVarD Int
           | LambdaD ExprD
           | AppD ExprD ExprD
    deriving (Show, Eq)

-- Define 'toExprD', which takes an 'Expr' and converts it into
-- an 'ExprD'.  Be careful: a variable may be represented as a
-- different index depending on how many lambdas it is nested by.
-- For example, "\x -> x (\y -> x)" is represented as:
--
--      LambdaD (AppD (BoundVarD 0) (LambdaD (BoundVarD 1)))
--                    ~~~~~~~~~~~~~          ~~~~~~~~~~~~~
--
-- You will need to define a helper function.  You may find useful this
-- standard library function, which returns the location of an element
-- in a list:
--
--      elemIndex :: a -> [a] -> Maybe Int
--

toExprD :: Expr -> ExprD
-- BEGIN toExprD (DO NOT DELETE THIS LINE)
toExprD = undefined
-- END toExprD (DO NOT DELETE THIS LINE)

-- Alpha equality on 'ExprD' is just structural equality.

alphaEqD :: ExprD -> ExprD -> Bool
alphaEqD e1 e2 = e1 == e2

-- Implement alpha equality on 'Expr':

alphaEq :: Expr -> Expr -> Bool
-- BEGIN alphaEq (DO NOT DELETE THIS LINE)
alphaEq = undefined
-- END alphaEq (DO NOT DELETE THIS LINE)

-- Here are some examples to test on. (Question: which of these
-- should alphaEq return True for?)

example_alphaEq1, example_alphaEq2, example_alphaEq3, example_alphaEq4 :: Bool
example_alphaEq1 = alphaEq "\\x y -> x" "\\y x -> y"
example_alphaEq2 = alphaEq "(\\x -> x) z" "(\\z -> z) x"
example_alphaEq3 = alphaEq "\\x -> \\z -> x" "\\y -> \\y -> y"
example_alphaEq4 = alphaEq "\\x -> y" "\\y -> y"

-------------------------------------------------------------------

--                  Substitution with a fresh supply

-------------------------------------------------------------------

-- The freshness constraint as stated in lecture is quite fiddly,
-- and requires a bit of computation in order to compute (we have
-- to compute the free variables of several expressions, and then
-- find a variable name which is distinct from any variable in
-- our set.)
--
-- An alternate strategy is to maintain a global counter i,
-- such that x_i, x_i+1, ...  are guaranteed to be fresh: every time a
-- name is allocated, this counter is bumped.  We can't do this
-- *directly* in Haskell, as Haskell is a pure language without
-- mutation.  Instead we must manually thread this global counter
-- through our code.  To avoid confusion, let's define a type synonym
-- for our counter, 'FreshSupply'.

type FreshSupply = Int

-- Every function that may need fresh names must accept a 'FreshSupply'
-- as its argument (so that it can tell what names are free), and return
-- a new 'FreshSupply' which is updated to exclude and names which were
-- used.  So for example, the function 'freshDumb' which allocates a
-- new fresh name takes a 'FreshSupply', and returns both a fresh
-- 'Name' (from 'FreshSupply'), as well as 'FreshSupply' + 1 (indicating
-- what the next new fresh name is.)  This function relies on an
-- invariant that no user-input variables start with a dollar sign
-- (unfortunately, this also means if you pretty-print these variables
-- they can't be parsed).

freshU :: FreshSupply -> (Name, FreshSupply)
freshU u = (Name ("$u" ++ show u), u+1)

-- Implement 'substU', which takes a 'FreshSupply', expression and
-- a substitution, and returns the substituted expression along with
-- a new 'FreshSupply'.  Do NOT use the 'fv' function.
--
-- How to manage the 'FreshSupply' variables?  Look at the types of
-- all of our functions: they all take a FreshSupply as an input,
-- and then produce a (new!) FreshSupply as output.  The rule is that
-- for any given FreshSupply, you should feed it into another function,
-- get back out a new FreshSupply, and then use that FreshSupply for
-- subsequent invocations.  Any given FreshSupply should be used exactly
-- once; e.g., it's not valid to say (freshU u, freshU u), since
-- the FreshSupply u is used twice.  Instead, you should say:
--
--   let u1 = freshU u
--       u2 = freshU u1
--   in ...
--
-- Here is a sample transcript with the reference implementation:
--
--  *Lambda> substU 0 "x" ("x", "y")
--  ("y",0)
--  *Lambda> substU 0 "\\x -> x" ("x", "y")
--  ("\\x -> x",0)
--  *Lambda> substU 0 "\\y -> \\z -> x" ("x", "y")
--  ("\\$u0 -> \\$u2 -> y",3)
--  *Lambda> substU 0 "\\y -> \\z -> x y z" ("x", "y")
--  ("\\$u0 -> \\$u2 -> y $u0 $u2",3)
--  *Lambda> substU 0 "(\\y -> x) (\\y -> x)" ("x", "y")
--  ("(\\$u0 -> y) (\\$u1 -> y)",2)
--
-- Your numbers do not have to match up exactly with the reference
-- implementation, but you should end up with alpha equivalent
-- terms.
--
-- Note for Experts: in an actual implementation, we would have
-- 'substU' take a *mapping* of substitutions, rather than a
-- single substitution, to avoid having to call 'subst' twice
-- in the lambda case.

substU :: FreshSupply -> Expr -> Subst -> (Expr, FreshSupply)
-- BEGIN substU (DO NOT DELETE THIS LINE)
substU = undefined
-- END substU (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  Testing

-------------------------------------------------------------------

-- For the next part, please write down three (or more) QuickCheck
-- properties testing 'alphaEq', 'subst' and/or 'substU'.  Your
-- properties should be only 1-2 lines long (not including type
-- signatures), or longer if you define a WELL-SPECIFIED helper
-- function (the helper function does something well defined and
-- easy to understand).  Your properties can take any number of 'Expr's and
-- 'Subst's as arguments, and QuickCheck will generate random inputs
-- for all of them.  For example, you could write a property with
-- the type 'Expr -> Subst -> Bool'.
--
-- We have also provided a reference implementation of substitution
-- on 'ExprD', under the name 'substD'; you can use any of these
-- functions in your QuickCheck properties.

-- Here is an example property to give you some idea of what kind of
-- properties we're looking for.  They don't have to be complicated!
prop_alphaEq_refl :: Expr -> Bool -- Alpha equivalence is symmetric
prop_alphaEq_refl e = alphaEq e e

-- A good QuickCheck property is one that can uncover bugs in your
-- implementation.  You might find this website useful if you want
-- to use some more features of QuickCheck:
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
-- (although there are plenty of properties which can be done in
-- "plain old style.")

-- BEGIN QuickCheck (DO NOT DELETE THIS LINE)
-- Your properties here
-- END QuickCheck (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  Reduction

-------------------------------------------------------------------

-- Enough work, time to play. Implement a function which takes
-- a CBN lambda term and evaluates for a single step.  As a reminder,
-- here are the small-step operational semantics for CBN:
--
--      (\x -> e1) e2  ---->  e1[x -> e2]
--
--                 e1  ---->  e1'
--              --------------------
--              e1 e2  ---->  e1' e2
--
-- 'step_cbn' is a recursive function; however, it should only do
-- ONE reduction at a time (the recursive call is purely to look
-- deeper into the expression to find a redex.)  CBN does NOT
-- evaluate under binders.
--
-- For example:
--  > step_cbn "(\\x -> (\\y -> x)) y"
--  Just "\\y1 -> y"

step_cbn :: Expr -> Maybe Expr
-- BEGIN step_cbn (DO NOT DELETE THIS LINE)
step_cbn = undefined
-- END step_cbn (DO NOT DELETE THIS LINE)

-- With a step function, we can write a function which takes an
-- expression, and returns the (possibly infinite) sequence of
-- reductions it takes until no more steps are possible.  For example:
--
--      reductions step_cbn "(\\x -> (\\z -> z) x) y"
--          == [ "(\\x -> (\\z -> z) x) y"
--             , "(\\z -> z) y"
--             , "y"
--             ] -- up to alpha equivalence
--
-- Implement this function.

reductions :: (Expr -> Maybe Expr) -> Expr -> [Expr]
-- BEGIN reductions (DO NOT DELETE THIS LINE)
reductions = undefined
-- END reductions (DO NOT DELETE THIS LINE)

-- Here are some example reductions:
--
-- > reductions step_cbn "x ((\\x -> x) y)"
-- ["x ((\\x -> x) y)"]
-- > reductions step_cbn "x"
-- ["x"]
-- > reductions step_cbn "(\\x -> (\\y -> x)) y"
-- ["(\\x -> \\y -> x) y","\\y1 -> y"]
-- > reductions step_cbn "(\\x -> x x) (\\x -> x x)"
-- ["(\\x -> x x) (\\x -> x x)","(\\x -> x x) (\\x -> x x)", ...

-------------------------------------------------------------------

--                  Other stuff

-------------------------------------------------------------------

instance Show Expr where
    show e = show (pprExpr e)

instance Show Name where
    show (Name x) = show x

instance IsString Expr where
    fromString = parseExpr

instance IsString Name where
    fromString = Name

prop_parsePpr :: Expr -> Bool
prop_parsePpr e = parseExpr (pprExpr e) == e

-- A pretty-printer.  The integer parameter to 'go' is the "precedence
-- level", we use it to control parenthesization.
pprExpr :: Expr -> String
pprExpr = go (0 :: Int)
    where go n e = paren n (go' e)
          go' (Var (Name x))      = (9, x)
          go' (App e1 e2)  = (6, go 5 e1 ++ " " ++ go 6 e2)
          go' (Lambda (Name x) e) = (3, "\\" ++ x ++ " -> " ++ go 2 e)
          paren n (m, s) | m > n     = s
                         | otherwise = "(" ++ s ++ ")"

parseExpr :: String -> Expr
parseExpr s =
  case readP_to_S exprParser s of
    [(a, "")] -> a
    _ -> error "parseExpr: failed"

exprParser :: ReadP Expr
exprParser = do e <- expr
                eof
                return e
    where expr = fun <++ app <++ nonApp
          fun =
            do _ <- string "\\"
               xs <- many1 (skipSpaces >> var)
               skipSpaces
               _ <- string "->"
               skipSpaces
               e <- expr
               return (foldr Lambda e xs)
          nonApp =
            between (char '(') (char ')') expr <++
            do x <- var
               return (Var x)
          app =
            do e  <- nonApp
               es <- many1 (skipSpaces >> nonApp)
               return (foldl App e es)
          var =
             do x <- satisfy (\c -> isLower c || c `elem` ("_" :: String))
                xs <- munch (\c -> isAlphaNum c || c `elem` ("\'_" :: String))
                return (Name (x:xs))

-- Thanks ehird! http://stackoverflow.com/q/9542313/23845
candidateNames :: [Name]
candidateNames = map Name ([1..] >>= (`replicateM` ['a'..'z']))

-- This Arbitrary instance relies on the Arbitrary instance for
-- Expr preferentially picking earlier candidate names.
instance Arbitrary Name where
    arbitrary = go candidateNames
      where
        go (c:cs) = frequency [(1, return c), (1, go cs)]
        go _ = error "Arbitrary Name exhausted"

instance Arbitrary Expr where
    arbitrary = go Set.empty
        where go :: Set Name -> Gen Expr
              go in_scope =
                sized $ \n ->
                  if n == 0
                    then frequency [(1, fmap Var free_var),
                                    (4, fmap Var bound_var)]
                    else resize (n `div` 2) $
                         frequency [(6, redex),
                                    (1, lambda free_var),
                                    (1, lambda bound_var),
                                    (1, app),
                                    (1, fmap Var free_var),
                                    (1, fmap Var bound_var)]
                where redex = do x <- free_var
                                 e1 <- go (Set.insert x in_scope)
                                 e2 <- go in_scope
                                 return (App (Lambda x e1) e2)
                      lambda gen_var = do x <- gen_var
                                          e <- go in_scope
                                          return (Lambda x e)
                      app = do e1 <- go in_scope
                               e2 <- go in_scope
                               return (App e1 e2)
                      free_var  = return (head (filter (`Set.notMember` in_scope) candidateNames))
                      bound_var
                        | Set.null in_scope = free_var
                        | otherwise = oneof (map return (Set.toList in_scope))
    shrink (Var _) = []
    shrink (Lambda x e) = [e] ++ [Lambda x e' | e' <- shrink e]
    shrink (App e1 e2) = [e1, e2] ++ [App e1' e2 | e1' <- shrink e1]
                                  ++ [App e1 e2' | e2' <- shrink e2]

check :: Testable prop => prop -> IO ()
check = quickCheckWith stdArgs {maxSuccess = 1000}

-------------------------------------------------------------------

--                  Reference implementation

-------------------------------------------------------------------

-- This section contains a reference implementation of substitution
-- on locally nameless terms, and a conversion function from 'ExprD'
-- to 'Expr'.
--
-- Do NOT use this implementation when implementing your substitution
-- functions.

isFreshU :: Name -> Bool
isFreshU (Name ('$':'u':_)) = True
isFreshU _ = False

fromExprD :: ExprD -> Expr
fromExprD e0 = go e0 (map (\i -> Name ("$u" ++ show i)) [(0::Int)..], [])
  where
    go (FreeVarD x)   _         = Var x
    go (BoundVarD i) (_,    xs) = Var (xs !! i)
    go (LambdaD e)   (u:us, xs) = Lambda u (go e (us, u:xs))
    go (LambdaD _)   ([],   _)  = error "fromExprD: exhausted name supply"
    go (AppD e1 e2)  uxs        = App (go e1 uxs) (go e2 uxs)

substD :: ExprD -> (Name, ExprD) -> ExprD
substD (FreeVarD x) (y, e)
    | x == y    = e
    | otherwise = FreeVarD x
substD (BoundVarD i) _ = BoundVarD i
substD (LambdaD e)   s = LambdaD (substD e s)
substD (AppD e1 e2)  s = AppD (substD e1 s) (substD e2 s)
