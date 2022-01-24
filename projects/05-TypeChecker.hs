{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef

import Data.String
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
import Control.Exception
import Debug.Trace

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as ReadP

import System.IO.Unsafe  -- Ho ho ho!
import qualified Language.Haskell.TH as TH

-- In this lab, we will implement a Hindley-Milner type inference
-- engine for uHaskell, a minimal subset of Haskell.  A side goal
-- of this lab is to get some experience writing *monadic* code in
-- Haskell; we will be using a monad to generate free type
-- variables (Lambda, ho!) and keep track of the constraints we
-- generate during the typechecking process.
--
-- You may find it convenient to call GHCi with the following flags:
--
--      ghci -XOverloadedStrings -XTemplateHaskell TypeChecker.hs
--
-- Along with overloaded strings, which we have used in previous
-- labs to write lambda expressions as strings (in this lab, types
-- play the same role), Template Haskell enables *quotes*, which
-- we will use to specify example programs with (we will literally
-- parse Haskell, and then translate it into uHaskell for you
-- to typecheck.)


-- We first recap by defining the data type for expressions in uHaskell.
-- It is very similar to the data type we worked with in the lambda
-- calculus lab; however, it's been extended with more surface language
-- features such as integers, booleans, pairs, if-then-else expressions
-- and lists, which are features supported by uHaskell.
--
-- If you try to print expressions, they will print as their raw Haskell
-- AST.  This can be somewhat unreadable, so we recommend instead using
-- 'ppr' to pretty-print the expression as if they were Haskell:
--
--      *Main> show (If (Bool True) (Var (Name "x")) (Var (Name "y")))
--      "If (Bool True) (Var \"x\") (Var \"y\")"
--      *Main> ppr (If (Bool True) (Var (Name "x")) (Var (Name "y")))
--      "if True then x else y"
--
-- 'ppr' will work on all of the other types we define in this lab.
-- Give it a try.

data Expr = Var Name            -- x
          | Int Int             -- 0, 1, 2, ...
          | Bool Bool           -- True, False
          | If Expr Expr Expr   -- if e1 then e2 else e3
          | Nil                 -- []
          | Lambda Name Expr    -- \x -> e
          | App Expr Expr       -- e1 e2
          | Pair Expr Expr      -- (e1, e2)
    deriving (Show)

newtype Name = Name String
    deriving (Ord, Eq)

-- We also must define a data type for patterns.  Patterns are the
-- binding expressions to the left of the equal sign, e.g., the
-- "(x:xs)" in "f (x:xs) = ...".  Patterns resemble their expression
-- brethren, but there are no computation forms (like lambdas and
-- function applications), only *constructors*, representing data
-- we can pattern match over.

data Pat = PVar Name            -- x
         | PPair Pat Pat        -- (p1, p2)
         | PCons Pat Pat        -- (p1:p2)
         | PNil                 -- []
    deriving (Show)

-- We put it all together in a data type for declarations
-- (e.g., "f (x:xs) = xs") and programs (multiple declarations
-- bundled together.)  Nothing surprising here.

data Decl = Decl Name Pat Expr
    deriving (Show)

newtype Program = Program [Decl]
    deriving (Show)


-- New to this lab, we must define a grammar of types which
-- uHaskell supports, e.g., Int -> Int.

data Type = TVar Name           -- a
          | TArrow Type Type    -- t1 -> t2
          | TInt                -- Int
          | TBool               -- Bool
          | TList Type          -- [t]
          | TPair Type Type     -- (t1, t2)
    deriving (Show, Eq)

-- As a warm up, please implement "occurs", which tests if a
-- given Name occurs in a Type.  For example, the name 'a'
-- occurs in the type 'a -> a', but not in the type 'Int'.

occurs :: Name -> Type -> Bool
-- BEGIN occurs (DO NOT DELETE THIS LINE)
occurs = undefined
-- END occurs (DO NOT DELETE THIS LINE)


-------------------------------------------------------------------

--                  Type inference by hand

-------------------------------------------------------------------

-- Before we implement type inference, let us do a simple example
-- by hand.
--
-- Consider the following Haskell program:
--
--      fix f = f (fix f)
--
-- Let us assign the following type variables for each sub-expression
-- in this program (you may find it helpful to draw the AST diagram,
-- as we did in class):
--
--      fix         :: tx
--      f           :: tf
--      f (fix f)   :: ta
--      fix f       :: tb
--
-- In the box below, write down the three constraints Hindley-Milner
-- type inference would generate.

{-
-- BEGIN inference_by_hand (DO NOT DELETE THIS LINE)
-- ANSWER HERE
-- END inference_by_hand (DO NOT DELETE THIS LINE)
-}

-- In the expression 'fix (\x -> if x then y else z)', what must the
-- type of 'y' be for this expression to typecheck?

{-
-- BEGIN fix_the_type_error (DO NOT DELETE THIS LINE)
-- ANSWER HERE
-- END fix_the_type_error (DO NOT DELETE THIS LINE)
-}

-------------------------------------------------------------------

--                  Constraint generation

-------------------------------------------------------------------

-- The first step of Hindley-Milner type inference is to generate
-- constraints. A Constraint is just a pair (t1, t2), specifying that t1
-- should unify with t2.

type Constraint = (Type, Type)

-- To assist in the constraint generation, we will pass
-- around an abstract 'TcEnv' (short for "type-checking environment").

type TcEnv = TcEnvImpl  -- abstract (you don't need to know how it's implemented)

-- We'll use TcEnv as **mutable state**: it will accumulate
-- constraints and generate fresh variables.  We could do
-- this entirely pure Haskell, but it would require quite a lot of
-- book-keeping (as you saw in previous labs).  Instead, we'll use the
-- IO monad to hide this book-keeping away from us.  (Note: We don't
-- technically need to use the IO monad; another monad, like the
-- State monad, would also suffice.)
--
-- TcEnv supports a few monadic operations:

-- 1. We can *mutably* assert that a constraint must hold using
--    addConstraint.  (Later, we'll collect all of the recorded
--    constraints and do constraint solving on them.)
addConstraint :: TcEnv -> Constraint -> IO ()

-- 2.  Get a fresh type variable.  This is helpful if you need to
--     produce a type, but you have no idea what it should be.  Generate
--     a fresh type, and constrain it later.
fresh :: TcEnv -> IO Type

-- So, for example, if you want to generate a fresh type variable,
-- and then assert it was equal to another type t2, you might write this
-- (assuming env :: TcEnv):
--
--      do t <- fresh env
--         addConstraint env (t, t2)
--
-- (NB: this particular example is kind of pointless, since you
-- could have just use t2 anywhere you would have used t.)


-- Write 'genPat', a recursive function that computes the 'Type' of an
-- 'Pat' given some context 'TcEnv', generating any type constraints
-- which we must solve for later.  We've provided the PVar case for
-- you.
--
-- Debugging tip: if you'd like to see what the value of a variable
-- at some point in your program, just print it statement, e.g.,
--
--      putStrLn (ppr t)
--
-- Things you can pretty-print this way include TcEnv, Pat, Expr and
-- Type.

genPat :: TcEnv -> Pat -> IO Type
genPat env (PVar n) = do
    -- PVar is a binding occurrence, so we don't actually know what
    -- the type of the variable n is!  So give it a fresh type.
    -- We are also secretly using TcEnv to record the set of variables
    -- *bound* by the pattern, for the next part of the lab.
    t <- fresh env
    addPatBinding env n t
    return t
-- BEGIN genPat (DO NOT DELETE THIS LINE)
genPat _ _ = undefined
-- END genPat (DO NOT DELETE THIS LINE)

-- To test your program on individual examples, we have provided
-- genPatTest function.

genPatTest :: Pat -> IO ()

-- Pass in the pattern you wish to test against
-- (e.g., (x:xs)) by wrapping it in the syntax (convertP [p| ... |])
-- (make sure you have passed -XTemplateHaskell to GHCi).  For
-- example:
--
--      *Main> genPatTest (convertP [p| (x:xs) |])
--      -- Pattern:
--      (x:xs) :: t1
--      -- Brings into scope:
--      --   x :: t0
--      --   xs :: t1
--      -- With constraints:
--      --   [t0] ~ t1
--
-- Your solution may not match this example exactly in type
-- variable naming or constraints generated; there are many
-- acceptable solutions.
--
-- (NB: The syntax [t0] ~ t1 means that the type [t0] must unify
-- with t1.  ~ is the conventional notation for "unifies with"
-- in Haskell, so we use it here.)


-- To write genExpr for expressions, we'll need TcEnv to play
-- double-duty as a **context**, keeping track of what names are in
-- scope, and what their types are (we didn't need to make use of this
-- functionality in genPat, because names bound in patterns aren't
-- accessible inside the pattern).  We provide two operations for
-- working with the context:

-- 1. Generate a new TcEnv (to, for example, pass to a recursive
--    subcall), where Name in scope with type Type.
addBinding :: TcEnv -> Name -> Type -> TcEnv

-- 2. From a TcEnv, look up the Type of a Name, or raise an
--    error if it is not in scope (this is why its return type
--    is in the IO monad!)
lookupBinding :: TcEnv -> Name -> IO Type

-- One property upheld by these two functions is that:
--
--      lookupBinding (addBinding env n t) n == return (Just t)
--
-- You'll make use of the context when typechecking lambda expressions
-- and variables, making use of the following helper functions.

-- Now, write 'genExpr', a recursive function that computes the 'Type'
-- of an 'Expr', given some context 'TcEnv', and adds any type
-- constraints that must hold between various types.

genExpr :: TcEnv -> Expr -> IO Type
-- BEGIN genExpr (DO NOT DELETE THIS LINE)
genExpr = undefined
-- END genExpr (DO NOT DELETE THIS LINE)

-- You can test your code using the genExprTest function.

genExprTest :: Expr -> IO ()

-- Pass in the expression you wish to test against
-- (e.g., \x -> 0 + x) by wrapping it in the syntax (convertE [e| ... |])
-- (make sure you have passed -XTemplateHaskell to GHCi).  For
-- example:
--
--      *Main> genExprTest (convertE [e| \x -> 0 + x |])
--      -- Expression:
--      \x -> (0 + x) :: t0 -> t2
--      -- With constraints:
--      --   t1 ~ t0 -> t2
--      --   Int -> Int -> Int ~ Int -> t1
--
-- Keep in mind that any variable that you use must be
-- brought into scope, e.g., by a lambda.


-- We've provided the definitions of infer for Decl and Program,
-- you don't have to worry about how these are implemented.

genDecl :: TcEnv -> Decl -> IO Type
genDecl env (Decl n p e) = do
    tp <- genPat env p
    m <- atomicModifyIORef' (tc_binds env) (\b -> (Map.empty, b))
    tf <- fresh env
    let env' = env { tc_ctx = Map.insert n tf (Map.union m (tc_ctx env)) }
    te <- genExpr env' e
    addConstraint env (tf, TArrow tp te)
    return (TArrow tp te)

genProgram :: TcEnv -> Program -> IO Type
genProgram env (Program ds) = do
    ts <- mapM (genDecl env) ds
    let go Nothing t = return (Just t)
        go (Just t') t = do
            addConstraint env (t, t')
            return (Just t')
    mb_t <- foldM go Nothing ts
    case mb_t of
        Nothing -> error "Must have at least one decl"
        Just t -> return t

-- You can test genExpr and genPat in the context of a whole program
-- using genProgramTest:

genProgramTest :: Program -> IO ()

-- Pass your program in using (convertD [d| ... |]).  We have provided some example
-- programs below:

ex_curried_add          = convertD [d| add x = \y -> x + y           |]
ex_id                   = convertD [d| id x = x                      |]
ex_if                   = convertD [d| ift b = \t -> \f ->
                                         if b then t else f          |]
ex_length               = convertD [d| length (x:xs) = 1 + length xs
                                       length [] = 0                 |]
ex_nested_binding       = convertD [d| f (x,y) = x + (\x -> y + x) 3 |]
ex_nested_binding2      = convertD [d| f (x,y) = (\x -> y x) 3 + x   |]
ex_nested_pattern       = convertD [d| f (x,y) = y + 1
                                       f ([],x) = 2                  |]
ex_plus3                = convertD [d| plus3 x = x + 3               |]
ex_redundant_pattern    = convertD [d| foo (x,[])      = 1
                                       foo ((x:xs),[]) = 1           |]
ex_uncurried_add        = convertD [d| uncurried_add (x,y) = x + y   |]

#if __GLASGOW_HASKELL__ >= 708
ex_occurs2              = convertD [d| f (x:xs) = \b ->
                                         if b then x else xs         |]
ex_occurs3              = convertD [d| f x = (x, f x)                |]
ex_occurs               = convertD [d| f x = x x                     |]
ex_pat_unify            = convertD [d| f ([], x)    = 1
                                       f ((x,y), z) = 2              |]
ex_unify                = convertD [d| foo (x:xs) = x + xs           |]
#else
-- Prior to GHC 7.8, GHC eagerly typechecks the contents of quotes.
-- This is no good for us, because we want some non-typechecking
-- examples.
ex_occurs2 = Program [Decl "f" (PCons (PVar "x") (PVar "xs")) (Lambda "b" (If (Var "b") (Var "x") (Var "xs")))]
ex_occurs3 = Program [Decl "f" (PVar "x") (Pair (Var "x") (App (Var "f") (Var "x")))]
ex_occurs = Program [Decl "f" (PVar "x") (App (Var "x") (Var "x"))]
ex_pat_unify = Program [Decl "f" (PPair PNil (PVar "x")) (Int 1),Decl "f" (PPair (PPair (PVar "x") (PVar "y")) (PVar "z")) (Int 2)]
ex_unify = Program [Decl "foo" (PCons (PVar "x") (PVar "xs")) (App (App (Var "+") (Var "x")) (Var "xs"))]
#endif

-- For example, test with:
--
--      *Main> genProgramTest ex_curried_add
--

-------------------------------------------------------------------

--                  Constraint solving

-------------------------------------------------------------------

-- Constraint solving consists of performing *unification* on our
-- constraints, successively refining our type variables until we have
-- solutions for everything.  To implement this, we'll need some helper
-- functions, provided below:

-- Substitution on types, constraints and maps are provided for you:

type Subst = (Name, Type)
subst :: Subst -> Type -> Type

substConstraint :: Subst -> Constraint -> Constraint
substConstraint s (a, b) = (subst s a, subst s b)

substMap :: Subst -> Map Name Type -> Map Name Type
substMap s = Map.map (subst s)

-- Constraint solving has two error conditions: occurs check
-- failure, and unification failure.  Call these functions to
-- report that such an error has occurred.  These functions raise
-- an exception in the IO monad.

failOccursCheck :: Name -> Type -> IO a
failOccursCheck x t = error $ "Occurs check failed: " ++ ppr x ++ " occurs in " ++ ppr t

failUnification :: Type -> Type -> IO a
failUnification t1 t2 = error $ "Unification failed: " ++ ppr t1 ++ " does not unify with " ++ ppr t2

-- Implement 'solveConstraints', a function that takes a list of
-- constraints, and performs unification on all of them, returning a
-- substitution from names to their fully unified types (the
-- "complete" mappings).
--
-- Hint: You should maintain both a list of constraints to be processed,
-- as well as the map of fully solved constraints (an invariant is that
-- every key of the solved constraint map shouldn't occur in any Type in
-- the list of constraints to solve, nor the types the solved
-- constraints map to.)
--
-- Hint: The reference solution is 20 lines.

solveConstraints :: [Constraint] -> IO (Map Name Type)
-- BEGIN solveConstraints (DO NOT DELETE THIS LINE)
solveConstraints = undefined
-- END solveConstraints (DO NOT DELETE THIS LINE)

-- Finally, we can put it all together!  To perform type inference, we
-- first generate constraints from our program.  Once we've done so, we
-- solve the constraints, giving us a substitution from type variables
-- to types.  We apply that substitution to the type of our program, and
-- we now have a fully inferred type!

typeInference :: Program -> IO Type
typeInference prog = do
    env <- initTcEnv
    t <- genProgram env prog
    constraints <- getConstraints env
    s <- solveConstraints constraints
    return (multiSubst s t)

-- Time to test.  We provided a test function for you
-- which takes an input program (e.g., ex_curried_add)
-- and Just type (if the program is well typed) or
-- Nothing (if the program is ill-typed), and checks
-- if this it matches.
--
-- You can use it like:
--
--      *Main> test (convertD [d| id x = x |]) (Just "a -> a")
--      OK
--

test :: Program -> Maybe Type -> IO ()
test p Nothing = m `Control.Exception.catch` ok
    where
        m = do
            t <- typeInference p
            putStrLn $ "FAILED: expected error, but inferred \"" ++ ppr t ++ "\""
        ok (ErrorCall _) = putStrLn "OK"
test p (Just expect_t) = do
    t <- typeInference p
    if t `typeEqual` expect_t
        then putStrLn "OK"
        else putStrLn $ "Failed: actual type \"" ++ ppr t ++ "\" does not match expected \"" ++ ppr expect_t ++ "\""

-- THIS WILL NOT BE GRADED.  Adjust the expected type in these tests to
-- verify that you have inferred the correct types.  This concludes the
-- lab!

all_tests :: IO ()
all_tests = mapM_ (uncurry test)
    [ (ex_curried_add,          Nothing)
    , (ex_id,                   Nothing)
    , (ex_if,                   Nothing)
    , (ex_length,               Nothing)
    , (ex_nested_binding,       Nothing)
    , (ex_nested_binding2,      Nothing)
    , (ex_nested_pattern,       Nothing)
    , (ex_plus3,                Nothing)
    , (ex_redundant_pattern,    Nothing)
    , (ex_uncurried_add,        Nothing)
    , (ex_occurs2,              Nothing)
    , (ex_occurs3,              Nothing)
    , (ex_occurs,               Nothing)
    , (ex_pat_unify,            Nothing)
    , (ex_unify,                Nothing)
    ]

-------------------------------------------------------------------

--                          Helper code

-------------------------------------------------------------------

-- You DO NOT have to read this section.

-- Implementation of TcEnv

data TcEnvImpl =
  TcEnv {
    tc_fresh :: IORef Int,
    tc_constraints :: IORef [Constraint],
    tc_binds :: IORef (Map Name Type),
    tc_ctx :: Map Name Type
  }

initTcEnv :: IO TcEnv
initTcEnv = do
    fresh <- newIORef 0
    constraints <- newIORef []
    binds <- newIORef Map.empty
    return TcEnv {
            tc_fresh = fresh,
            tc_constraints = constraints,
            tc_binds = binds,
            tc_ctx = Map.fromList [
                        (Name "+", TArrow TInt (TArrow TInt TInt)),
                        (Name "==", TArrow TInt (TArrow TInt TBool))
                     ]
        }

addBinding env n t =
    env { tc_ctx = Map.insert n t (tc_ctx env) }

lookupBinding env n =
    case Map.lookup n (tc_ctx env) of
        Nothing -> error $ "Variable " ++ show n ++ " is not in scope"
        Just t -> return t

fresh env =
    fmap intToFreshType (atomicModifyIORef' (tc_fresh env) (\i -> (i+1, i)))

addPatBinding :: TcEnv -> Name -> Type -> IO ()
addPatBinding env n t = do
    m <- readIORef (tc_binds env)
    when (Map.member n m) $
        error $ "Cannot shadow variable " ++ show n ++ " in pattern"
    writeIORef (tc_binds env) (Map.insert n t m)

intToFreshType :: Int -> Type
intToFreshType i = TVar (Name ("t" ++ show i))

addConstraint env c =
    atomicModifyIORef' (tc_constraints env) (\cs -> (c:cs, ()))

getConstraints :: TcEnv -> IO [Constraint]
getConstraints env = readIORef (tc_constraints env)

-- Substitutions on types

subst (x, v) t = multiSubst (Map.singleton x v) t

multiSubst m (TVar x)
    | Just t <- Map.lookup x m = t
    | otherwise = TVar x
multiSubst m (TArrow t1 t2)
    = TArrow (multiSubst m t1) (multiSubst m t2)
multiSubst m TInt = TInt
multiSubst m TBool = TBool
multiSubst m (TList t) = TList (multiSubst m t)
multiSubst m (TPair t1 t2) = TPair (multiSubst m t1) (multiSubst m t2)

-- Testing functions

genPatTest e = do
    env <- initTcEnv
    r <- genPat env e
    putStrLn $ "-- Pattern:"
    putStrLn $ pprPat e ++ " :: " ++ pprType r
    bs <- readIORef (tc_binds env)
    when (not (Map.null bs)) $ putStrLn $ "-- Brings into scope:"
    mapM_ (\(n, t) -> putStrLn $ "--   " ++ pprName n ++ " :: " ++ pprType t) (Map.toList bs)
    printConstraints env

genExprTest e = do
    env <- initTcEnv
    r <- genExpr env e
    putStrLn $ "-- Expression:"
    putStrLn $ pprExpr e ++ " :: " ++ pprType r
    printConstraints env

genProgramTest e = do
    env <- initTcEnv
    r <- genProgram env e
    putStrLn $ "-- Program: "
    putStrLn $ ppr (getProgramName e) ++ " :: " ++ ppr r
    printConstraints env

printConstraints :: TcEnv -> IO ()
printConstraints env = do
    cs <- readIORef (tc_constraints env)
    when (not (null cs)) $ putStrLn $ "-- With constraints:"
    mapM_ (\(t1, t2) -> putStrLn $ "--   " ++ pprType t1 ++ "  ~  " ++ pprType t2) cs

getProgramName (Program []) = error "no program name"
getProgramName (Program ((Decl n _ _):_)) = n

-- Equality on types

typeFreeVars :: Type -> [Name]
typeFreeVars (TVar n) = [n]
typeFreeVars (TArrow t1 t2) = typeFreeVars t1 ++ typeFreeVars t2
typeFreeVars TInt = []
typeFreeVars TBool = []
typeFreeVars (TList t) = typeFreeVars t
typeFreeVars (TPair t1 t2) = typeFreeVars t1 ++ typeFreeVars t2

typeEqual :: Type -> Type -> Bool
typeEqual t1 t2 =
    alphaEquiv 0 Map.empty (typeFreeVars t1) Map.empty (typeFreeVars t2) &&
    typeStructEqual t1 t2
  where
    alphaEquiv i mx (x:xs) my (y:ys)
        | Map.lookup x mx == Map.lookup y my
        = alphaEquiv (i+1) (Map.insert x i mx) xs (Map.insert y i my) ys
        | otherwise
        = False
    alphaEquiv _ _ [] _ [] = True
    alphaEquiv _ _ _ _ _ = False

    typeStructEqual (TVar _) (TVar _) = True
    typeStructEqual (TArrow t1 t2) (TArrow t1' t2') = typeStructEqual t1 t1' && typeStructEqual t2 t2'
    typeStructEqual TInt TInt = True
    typeStructEqual TBool TBool = True
    typeStructEqual (TList t) (TList t') = typeStructEqual t t'
    typeStructEqual (TPair t1 t2) (TPair t1' t2') = typeStructEqual t1 t1' && typeStructEqual t2 t2'
    typeStructEqual _ _ = False

-- Code for pretty-printing uHaskell

pprName :: Name -> String
pprName (Name x) = x

pprExpr :: Expr -> String
pprExpr = go (0 :: Int)
    where go _ (Var (Name x))
            | (c:_) <- x
            , isLower c || c == '_'  = x
            | otherwise = "(" ++ x ++ ")"
          go _ (Int i)             = show i
          go _ (Bool b)            = show b
          go n (If c t e)          =
            parens n $ "if " ++ go (n+1) c ++
                       " then " ++ go (n+1) t ++
                       " else " ++ go (n+1) e
          go _ Nil                 = "[]"
          go n (Lambda (Name x) e) =
            parens n $ "\\" ++ x ++ " -> " ++ go (n+1) e
          go n (App (App (Var (Name "+")) e1) e2) = parens n $ go (n+1) e1 ++ " + " ++ go (n+1) e2
          go n (App (App (Var (Name "==")) e1) e2) = parens n $ go (n+1) e1 ++ " == " ++ go (n+1) e2
          go n (App e1 e2)         =
            parens n $ (go (n+1) e1) ++ " " ++ (go (n+1) e2)
          go n (Pair e1 e2)        = "(" ++ go 0 e1 ++ ", " ++ go 0 e2 ++ ")"
          parens n s | n > 0     = "(" ++ s ++ ")"
                     | otherwise = s

pprPat :: Pat -> String
pprPat = go
    where go (PVar (Name x)) = x
          go (PPair p1 p2) = "(" ++ go p1 ++ ", " ++ go p2 ++ ")"
          go (PCons p1 p2) = "(" ++ go p1 ++ ":" ++ go p2 ++ ")"
          go PNil = "[]"

pprDecl :: Decl -> String
pprDecl (Decl f p e) = pprName f ++ " " ++ pprPat p ++ " = " ++ pprExpr e

pprProgram :: Program -> String
pprProgram (Program ds) = intercalate "\n" (map pprDecl ds)

pprType :: Type -> String
pprType = go (-1)
  where
    go n = f
      where
        f (TVar (Name x)) = x
        f (TArrow t1 t2) = parens 1 (go 1 t1 ++ " -> " ++ go 0 t2)
        f TInt = "Int"
        f TBool = "Bool"
        f (TList t) = "[" ++ go 0 t ++ "]"
        f (TPair t1 t2) = "(" ++ go 0 t1 ++ ", " ++ go 0 t2 ++ ")"
        parens m s | m > n = s
                   | otherwise = "(" ++ s ++ ")"

-- Default to showing pretty-printed representations when we print
-- expressions.

class Pretty a where
    ppr :: a -> String

instance Pretty Expr where ppr = pprExpr
instance Pretty Pat where ppr = pprPat
instance Pretty Program where ppr = pprProgram
instance Pretty Name where ppr = pprName
instance Pretty Type where ppr = pprType

instance Show Name where
    show (Name n) = show n

instance Show TcEnv where
    show env = show (tc_ctx env)

instance Pretty TcEnv where
    ppr = show -- I'm being lazy

instance IsString Name where
    fromString = Name

instance IsString Type where
    fromString = parseType

-- Reuse Haskell's parser to parse our program formats.  We can
-- get this capability using Template Haskell *quotations*, which
-- reify Haskell syntax into a full AST corresponding to Haskell.
-- We then convert the fragment of this AST supported by uHaskell
-- into the uHaskell syntax you work with in this lab.

thToProgram :: [TH.Dec] -> Program
thToProgram ds = Program (concatMap thToDecls ds)

thToDecls :: TH.Dec -> [Decl]
thToDecls (TH.FunD n cs) = map (thToDecl n) cs
thToDecls d = error $ "Unsupported decl form " ++ show d

thToDecl :: TH.Name -> TH.Clause -> Decl
thToDecl n (TH.Clause [pat] (TH.NormalB e) []) =
    Decl (thToName n) (thToPat pat) (thToExpr e)
thToDecl _ c = error $ "Unsupported clause form " ++ show c

thToName :: TH.Name -> Name
thToName = Name . TH.nameBase

thToPat :: TH.Pat -> Pat
thToPat (TH.VarP n) = PVar (thToName n)
thToPat (TH.TupP [p1, p2]) = PPair (thToPat p1) (thToPat p2)
thToPat (TH.ConP n []) | n == '[] = PNil
thToPat (TH.InfixP p1 n p2) | n == '(:) = PCons (thToPat p1) (thToPat p2)
thToPat p = error $ "Unsupported pat form " ++ show p

thToExpr :: TH.Exp -> Expr
thToExpr (TH.VarE n) = Var (thToName n)
thToExpr (TH.LitE (TH.IntegerL i)) = Int (fromInteger i)
thToExpr (TH.ConE n) | n == 'True = Bool True
                     | n == 'False = Bool False
                     | n == '[] = Nil
thToExpr (TH.CondE c t f) = If (thToExpr c) (thToExpr t) (thToExpr f)
thToExpr (TH.LamE [TH.VarP n] e) = Lambda (thToName n) (thToExpr e)
thToExpr (TH.AppE e1 e2) = App (thToExpr e1) (thToExpr e2)
-- https://hackage.haskell.org/package/template-haskell-2.16.0.0/changelog
#if __GLASGOW_HASKELL__ >= 810
thToExpr (TH.TupE [Just e1, Just e2]) = Pair (thToExpr e1) (thToExpr e2)
#else
thToExpr (TH.TupE [e1, e2]) = Pair (thToExpr e1) (thToExpr e2)
#endif
thToExpr (TH.InfixE (Just e1) (TH.VarE n) (Just e2)) =
    App (App (Var (thToName n)) (thToExpr e1)) (thToExpr e2)
thToExpr e = error $ "Unsupported expr form " ++ show e

convertE :: TH.Q TH.Exp -> Expr
convertE = thToExpr . unsafePerformIO . TH.runQ

convertP :: TH.Q TH.Pat -> Pat
convertP = thToPat . unsafePerformIO . TH.runQ

convertD :: TH.Q [TH.Dec] -> Program
convertD = thToProgram . unsafePerformIO . TH.runQ

-- Parsing types (we cannot use TH for them, as we would have
-- to explicitly provide quantifiers)

typeParser :: ReadP Type
typeParser = app
    where
        app = do
            t <- nonApp
            ((do skipSpaces
                 string "->"
                 skipSpaces
                 t2 <- app
                 return (TArrow t t2)) <++ return t)
        nonApp = paren <++ list <++ bool <++ int <++ var
        bool = string "Bool" >> return TBool
        int = string "Int" >> return TInt
        var = fmap TVar name
        list = fmap TList $ between (char '[' >> skipSpaces) (skipSpaces >> char ']') app
        paren = between (char '(' >> skipSpaces) (skipSpaces >> char ')') comma_or_app
        comma_or_app = do
            t1 <- app
            ((do skipSpaces
                 char ','
                 skipSpaces
                 t2 <- app
                 return (TPair t1 t2))
                <++ return t1)
        name = fmap Name (munch1 (\c -> isLower c || isNumber c))

parseType :: String -> Type
parseType s =
  case readP_to_S (typeParser >>= \r -> eof >> return r) s of
    [(a, "")] -> a
    rs -> error $ "parseType: failed " ++ show rs
