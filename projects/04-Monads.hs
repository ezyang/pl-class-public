{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Monads where

import Control.Monad.State hiding ((>=>))
import Control.Applicative
import Control.Monad hiding ((>=>))

-----------------------------------------------------------------------

--                          Maybe monad

-----------------------------------------------------------------------

-- Recall Calculator.hs.  In it, you wrote a step function to interpret
-- instructions, and a run function which executed instructions
-- one-by-one until there were none left.

type Stack = [Int]
data Instr = IPlus
           | IPush Int
    deriving (Show)

step :: Instr -> Stack -> Maybe Stack
step IPlus (x:y:rest) = Just (x + y:rest)
step (IPush n) rest = Just (n:rest)
step _ _ = Nothing

run0 :: [Instr] -> Stack -> Maybe Stack
run0 (i:is) stk =
    case step i stk of
        Just stk' -> run0 is stk'
        Nothing -> Nothing
run0 [] stk = Just stk

-- Maybe is a monad: a Maybe action either fails and short-circuits
-- (with Nothing) or succeeds and continues.  Rewrite 'run' using monads
-- (using either do notation or bind).  Your solution for this
-- exercise should NOT use case on a Maybe, Nothing or Just.
--
-- As a reminder, here are the types of (>>=) and return under
-- the Maybe monad:
--
--      (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--      return :: a -> Maybe a

run :: [Instr] -> Stack -> Maybe Stack
-- BEGIN run (DO NOT DELETE THIS LINE)
run = undefined
-- END run (DO NOT DELETE THIS LINE)

-----------------------------------------------------------------------

--                          State monad

-----------------------------------------------------------------------

-- Recall your 'substU' function from Lambda.hs.  In it, you manually
-- threaded a FreshSupply into and out of recursive calls to substU.

data Expr = Var Name
          | Lambda Name Expr
          | App Expr Expr
    deriving (Show, Eq)
type Name = String
type Subst = (Name, Expr)
type FreshSupply = Int

freshU0 :: FreshSupply -> (Name, FreshSupply)
freshU0 u = ("$u" ++ show u, u+1)

substU0 :: FreshSupply -> Expr -> Subst -> (Expr, FreshSupply)
substU0 u (Var x) (x', e) =
    if x == x'
        then (e, u)
        else (Var x, u)
substU0 u (App e1 e2) s =
    let (e1', u') = substU0 u e1 s
        (e2', u'') = substU0 u' e2 s
    in (App e1' e2', u'')
substU0 u (Lambda x e) s =
    let (y, u') = freshU0 u
        (e', u'') = substU0 u' e (x, Var y)
        (e'', u''') = substU0 u'' e' s
    in (Lambda y e'', u''')

-- This pattern can be abstracted into a "state" monad, which handles
-- threading the state through calls.

type UniqMonad0 a = FreshSupply -> (a, FreshSupply)

-- To make it a monad, we have to define two operations.
-- (We've also given you versions of the types with the
-- type synonym expanded.)  Intuitively, 'bindU' has the
-- job of plumbing a state parameter into the first
-- argument, and then resulting state into the second
-- argument.

--  returnU :: a -> (FreshSupply -> (a, FreshSupply))
returnU :: a -> UniqMonad0 a
-- BEGIN returnU (DO NOT DELETE THIS LINE)
returnU = undefined
-- END returnU (DO NOT DELETE THIS LINE)

--  bindU :: (FreshSupply -> (a, FreshSupply))
--        -> (a -> (FreshSupply -> (b, FreshSupply)))
--        -> (FreshSupply -> (b, FreshSupply))
bindU :: UniqMonad0 a -> (a -> UniqMonad0 b) -> UniqMonad0 b
-- BEGIN bindU (DO NOT DELETE THIS LINE)
bindU = undefined
-- END bindU (DO NOT DELETE THIS LINE)

-- Unfortunately, to actually define a monad instance, we can't
-- use the type 'UniqMonad0' directly; we have to define a new
-- data type to represent the monad.  Here, we've gone ahead and
-- written the necessary plumbing to make this work.

newtype UniqMonad a
    = UniqMonad { runUniqMonad :: FreshSupply -> (a, FreshSupply) }

-- In GHC 7.10, we also have to define Functor and Applicative

instance Functor UniqMonad where
    fmap = liftM

instance Applicative UniqMonad where
    pure  = return
    (<*>) = ap

instance Monad UniqMonad where
    return x = UniqMonad (returnU x)
    m >>= f = UniqMonad (bindU (runUniqMonad m) (runUniqMonad . f))

freshU :: UniqMonad Name
freshU = UniqMonad (\u -> ("$u" ++ show u, u+1))

-- Rewrite substU using monads.  Specifically, you should not have
-- any variable in scope which has type FreshSupply.

substU :: Expr -> Subst -> UniqMonad Expr
-- BEGIN substU (DO NOT DELETE THIS LINE)
substU = undefined
-- END substU (DO NOT DELETE THIS LINE)

-- To run substU, use this function:
--
--      runUniqMonad :: UniqMonad a -> FreshSupply -> (a, FreshSupply)
--
-- e.g., runUniqMonad (substU (Var "x") ("x", (Var "y"))) 0

-----------------------------------------------------------------------

--                          Combinators

-----------------------------------------------------------------------

-- In lecture, we mentioned the monad laws:
--
--  return x >>= f                  === f x
--  m >>= return                    === m
--  m >>= (\x -> m2 >>= \y -> m3)   === (m1 >>= (\x -> m2)) >>= \y -> m3
--
-- You might think that these laws are weirdly asymmetric, and they
-- are!  Another way of expressing the monad laws is in terms of the
-- monad composition operator >=>.  This operator has the following
-- properties:
--
--  return >=> f        === f
--  f >=> return        === f
--  (f >=> g) >=> h     === f >=> (g >=> h)
--
-- Define (>=>) in terms of (>>=) (do NOT use do notation):

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- BEGIN (>=>) (DO NOT DELETE THIS LINE)
(>=>) = undefined
-- END (>=>) (DO NOT DELETE THIS LINE)

-- In fact, (>=>) is equivalent in expressivity to (>>=).  To
-- show this, define (>>=) using ONLY (>=>).

bind :: Monad m => m a -> (a -> m b) -> m b
-- BEGIN bind (DO NOT DELETE THIS LINE)
bind = undefined
-- END bind (DO NOT DELETE THIS LINE)
