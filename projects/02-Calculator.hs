{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 708
{-# LANGUAGE OverlappingInstances #-}
#endif
module Calculator where

-- In this lab, we will start exploring some of the implementation
-- themes of this course by writing a simple interpreter for an
-- arithmetic language, a very small subset of Haskell, and then
-- compiling this language into a simple stack based instruction
-- language.
--
-- This is NOT a heavy coding lab: the solution for each given problem
-- should only be a few lines of code.
--
-- The recommended way to run this code is to load it into ghci:
--
--      ghci Calculator.hs
--
-- You will then be able to run most functions directly in the resulting
-- REPL.  If you make modifications to this file, type ':r' to reload
-- your changes in the repl.  Note that you will lose any local bindings
-- when you reload, so if you want some to persist put it in this file.
--
-- The installation of Haskell on 'access.cims.nyu.edu' (accessible using 'ssh
-- username@access.cims.nyu.edu') has QuickCheck preinstalled and this is
-- all you need to do; if you are using your own installation of Haskell
-- you may have to install QuickCheck.  If you have Cabal installed,
-- you can install QuickCheck using 'cabal install --lib QuickCheck' (
-- if the --lib flag is not recognized, run 'cabal install QuickCheck'
-- instead).
--
-- (Guru meditation: This lab should work with GHC 7.6 or later, and
-- with QuickCheck 2.6 or later, and possibly more.  Let us know if
-- you run into compatibility bugs, however.)

import Test.QuickCheck
import Test.QuickCheck.Gen

import Debug.Trace

-------------------------------------------------------------------

--                  The expression language

-------------------------------------------------------------------

-- The expression language we'd like to define is very simple:
--
--      b ::= + | - | *     -- as before!
--      e ::= e b e | n
--
-- Here's a data type for this language.  Before you continue, make sure
-- you can understand the correspondence between the grammar and the
-- data types we've written out here.

data BinOp = Plus | Minus | Times
    deriving (Show, Eq)

data Expr = Op BinOp Expr Expr
          | Lit Int
    deriving (Show, Eq)

-- Here are some handy utility functions for constructing example
-- expressions.

plus, minus, times :: Expr -> Expr -> Expr
plus = Op Plus
minus = Op Minus
times = Op Times

example :: Expr
example = times (plus (Lit 1) (Lit 2)) (Lit 3)

-- Enough of syntax, what is the meaning of a binary operator?  A simple
-- answer is that it simply represents the corresponding Haskell
-- (metalanguage) operator.  Here is a simple (higher order!) function
-- which interprets the meaning of a binary operator.

interpBinOp :: BinOp -> (Int -> Int -> Int)
interpBinOp Plus  = (+) -- This is Haskell notation to take an infix operator
interpBinOp Minus = (-) -- and turn it into a normal non-infix "symbol"
interpBinOp Times = (*)

-- We can write an interpreter for these expressions, which takes an
-- arithmetic expression and evaluates it to an integer.  (You might
-- like to use interpBinOp; also, feel free to change the top-level
-- declaration as you like, e.g. to add pattern matching on arguments.)

interp :: Expr -> Int
-- BEGIN interp (DO NOT DELETE THIS LINE)
interp = undefined
-- END interp (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  A simple optimizer

-------------------------------------------------------------------

-- Now we are going to write a simple optimizer for our expression
-- language.  Technically, an optimizer is a bit useless for the
-- particular language we have here, as we can always evaluate an
-- expression to a number. However, it is not difficult to imagine a
-- variant of our arithmetic language that has variables as well, in
-- which case an optimizer would be useful.
--
-- We'll write a very simple optimizer which optimizes expressions of
-- the form "e + 0" to "e".  To start off, write a PATTERN for this
-- function which matches expressions which look like "e + 0" and
-- optimizes them to "e". We DO NOT want you to attempt to optimized
-- nested occurrences of this expression.
--
-- A real world peephole optimizer will include more algebraic
-- identities than just e + 0. If you like, you can add some more;
-- however, for the purposes of this lab, an implementation of
-- simplifyZero that ONLY optimizes additions by zero is acceptable.

simplifyZero :: Expr -> Expr
-- BEGIN simplifyZero (DO NOT DELETE THIS LINE)
simplifyZero e {- replace this pattern -} = e
-- END simplifyZero (DO NOT DELETE THIS LINE)
simplifyZero e = e

-- No need to edit this now; you'll fix it later
simplifier :: Expr -> Expr
-- BEGIN simplifier (DO NOT DELETE THIS LINE)
simplifier = simplifyZero
-- END simplifier (DO NOT DELETE THIS LINE)

-- You have probably been testing your programs correctness by running them
-- on simple examples.  With QuickCheck, we can instead define
-- properties that are programs should fulfill, and then test them
-- on randomly generated inputs.
--
-- Here are three properties we might want our optimizer to uphold:
--
--      1. (Correctness) If we run the unoptimized and optimized
--      versions, we should get the same result.
--
--      2. (Idempotency) Running the optimizer twice is equivalent to
--      running it once.
--
--      3. (Optimality) After running the optimizer, there should be
--      no more instances of the pattern that we looked for.
--
-- Please implement these three predicates using 'simplifier'.
-- You can then test your program by writing 'check
-- prop_optimizer_correctness' (or another property) in your GHCi
-- session; you can also test the correctness of your properties by
-- introducing bugs to the implementation and seeing if the properties
-- fail.
--
-- Some of these properties will fail.  This is expected; read on!

prop_optimizer_correctness :: Expr -> Bool
-- BEGIN prop_optimizer_correctness (DO NOT DELETE THIS LINE)
prop_optimizer_correctness = undefined
-- END prop_optimizer_correctness (DO NOT DELETE THIS LINE)

prop_optimizer_idempotent :: Expr -> Bool
-- BEGIN prop_optimizer_idempotent (DO NOT DELETE THIS LINE)
prop_optimizer_idempotent = undefined
-- END prop_optimizer_idempotent (DO NOT DELETE THIS LINE)

-- For property (3), we've written the following helper function
-- to look for instances where optimization can occur. 'findPlusZero'
-- returns 'True' if there are any occurrences of e + 0 in an
-- expression, and 'False' otherwise.  Please use it in your
-- QuickCheck property.
findPlusZero :: Expr -> Bool
findPlusZero (Lit _)             = False
findPlusZero (Op Plus e (Lit 0)) = True
findPlusZero (Op _ e1 e2)        = findPlusZero e1 || findPlusZero e2

prop_optimizer_optimizes :: Expr -> Bool
-- BEGIN prop_optimizer_optimizes (DO NOT DELETE THIS LINE)
prop_optimizer_optimizes = undefined
-- END prop_optimizer_optimizes (DO NOT DELETE THIS LINE)

-- You may have noticed that property (3) is failing (if you
-- are unlucky, property (2) may be failing too!), and QuickCheck
-- is reporting quite a large randomly generated test input which
-- induced the failure.  How should you go about debugging this problem?
-- One approach is to copy paste the failing input into your GHCi
-- session, and manually run your implementation with some tracing to
-- try to figure out what's going on.
--
-- A better approach is to try to *minimize* the test case before trying
-- to figure out what the bug is.  For example, if your test case is "Op
-- Plus e1 e2", where e1 and e2 are some large expressions you might try
-- testing on just e1.  Or you might try and shrink e1 to some smaller
-- e1', and check if "Op Plus e1' e2" still exhibits the error.  So
-- your workflow would be something like this:
--
-- To shrink a failing test case e:
--  1. Generate some candidate smaller test case e' (by removing
--     subexpressions, etc.)
--  2. Test the property on e'
--  3. If it still FAILS, recursively shrink e'.
--     If it PASSES, go back to step 1 and generate a different candidate
--     case.  If you run out of candidates, stop and return the smallest
--     still failing test case.
--
-- QuickCheck has implemented this algorithm: all you have to do tell
-- QuickCheck how to do step (1): given a failing test case
-- e, generate a list of candidate smaller test cases.
--
-- In this file, we've told QuickCheck to use 'expr_shrink' to carry out
-- step (1).  Implement 'expr_shrink', and test it by running 'check
-- prop_optimizer_optimizes' and seeing if your test cases get smaller.
-- (Hint: you may find a list comprehension useful here.)
--
-- If you can't figure out how to do this, feel free to skip ahead;
-- it's not necessary for the rest of the lab.

expr_shrink :: Expr -> [Expr]
-- BEGIN expr_shrink (DO NOT DELETE THIS LINE)
expr_shrink e = []
-- END expr_shrink (DO NOT DELETE THIS LINE)

-- At this point, it should be clear that the property fails
-- because simplifyZero will only optimize an expression like 2 + 0, and
-- not a more complex one like 2 + (2 + 0), since the addition of zero
-- occurs in a nested expression; however, our property wants all nested
-- occurrences to be simplified.  How should we fix this? We could
-- rewrite this function to recursively look into subexpressions and
-- attempt to optimize them too, but a better strategy is to define a
-- HIGHER ORDER function which does this for us.
--
-- 'peephole' is a higher order function that takes an optimizer
-- (like simplifyZero) and recursively applies it to every
-- subexpression of an expression.  If 'simplifyZero' is an optimizer
-- which only looks at the top-most expression, 'peephole simplifyZero'
-- is an optimizer which looks at all sub-expressions.
--
-- (Hint: if your implementation of peephole infinite loops, try using
-- the 'trace' function to get more insight into what is going on.)

peephole :: (Expr -> Expr) -> Expr -> Expr
-- BEGIN peephole (DO NOT DELETE THIS LINE)
peephole = undefined
-- END peephole (DO NOT DELETE THIS LINE)

-- Once you are done, update your QuickCheck tests to use the
-- peephole'ified optimizer, and verify that they are all passing.

-------------------------------------------------------------------

--                  The instruction language

-------------------------------------------------------------------

-- Let's talk about the simple instruction language that you will be
-- compiling your expressions to.  This language is defined by the
-- following grammar:
--
--      b ::= + | - | *
--      i ::= OP b | PUSH n
--      is ::= i; is | {- empty -}          -- this is a list

data Instr = IOp BinOp
           | IPush Int
    deriving (Show, Eq)

-- What is the meaning of an instruction?  We are defining a stack
-- machine, so instructions do one of two things:
--
--  1. They "PUSH" a literal value onto the stack, or
--  2. They pop two values off the stack, do an "OP"eration on them,
--  and then pushes the result onto the stack.  Note that this could
--  FAIL if there are not enough values on the stack (a stack underflow
--  error).
--
-- For example, consider the following program:
--
--      PUSH 2; PUSH 1; OP -; PUSH 3; OP *
--
-- After running the first two instructions, we have a stack consisting
-- of [1, 2].  Now, we run the minus OP, which pops two elements
-- off the stack and subtracts them.  In this lab, we will define the
-- operation so that it subtracts the top value from the next value
-- (this is an arbitrary choice; you can just as easily do it the
-- other way; we choose this convention as it matches with the stack
-- machines of many other well known runtimes like the JVM).
-- This gives us 1, which we push back onto the stack, giving us a new
-- stack [1].  Push 3 to get [3, 1], and then multiplication gives us a
-- final stack of [3].
--
-- We'll represent stacks as just lists of integers, where the head
-- of the list is the top of the stack.  In Haskell, lists are pure
-- data structures which cannot be mutated; we will "mutate" the
-- stack by returning a *new* stack from a function which represents
-- the updated stack.

type Stack = [Int]

-- We can interpret an instruction as taking a stack, performing
-- some operation on it, and returning the new stack after the
-- operation, but possibly failing.  Please implement 'step', which
-- does this.

step :: Instr -> Stack -> Maybe Stack
-- BEGIN step (DO NOT DELETE THIS LINE)
step = undefined
-- END step (DO NOT DELETE THIS LINE)

-- We should also tie this together, and write a function that
-- takes a stream of instructions and an initial stack, and executes
-- the instructions one by one until there are no more left.

run :: [Instr] -> Stack -> Maybe Stack
-- BEGIN run (DO NOT DELETE THIS LINE)
run = undefined
-- END run (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  A simple compiler

-------------------------------------------------------------------

-- Finally, we are ready to implement a compiler for our expression
-- language.  This compiler takes an AST, and compiles it into a
-- sequence of instructions which, when evaluated on an empty
-- stack, results in a stack with one element that is equal to
-- the result of interpreting the expression directly. (You'll get
-- no points for cheekily emitting a single PUSH instruction!) Also,
-- write a QuickCheck property to test this.
--
-- (Hint: This is a recursive function, and it can be implemented in
-- two lines.)

compile :: Expr -> [Instr]
-- BEGIN compile (DO NOT DELETE THIS LINE)
compile = undefined
-- END compile (DO NOT DELETE THIS LINE)

-- Your compiler is correct if running the instructions produced
-- by the compiler on an empty stack gives you the same result as
-- directly evaluating the expression.

prop_compile_correctness :: Expr -> Bool
-- BEGIN prop_compile_correctness (DO NOT DELETE THIS LINE)
prop_compile_correctness = undefined
-- END prop_compile_correctness (DO NOT DELETE THIS LINE)

-- We've also provided for you a simple decompiler which takes
-- a stream of instructions and translates it back into an expression
-- that would generate it if possible.  Write a QuickCheck test
-- which asserts that decompiling the output of your compiler
-- results in the original expression, and vice versa.
-- (Hint: It's not an error if the decompiler fails to decompile
-- an instruction stream; just don't test the invariant in that case.)

decompile :: [Instr] -> Maybe Expr
decompile is =
    case decompile' is [] of -- decompile' is defined in the utility
        Just [e] -> Just e   -- section of this lab
        _ -> Nothing

prop_compile_decompile :: Expr -> Bool
-- BEGIN prop_compile_decompile (DO NOT DELETE THIS LINE)
prop_compile_decompile = undefined
-- END prop_compile_decompile (DO NOT DELETE THIS LINE)

prop_decompile_compile :: [Instr] -> Bool
-- BEGIN prop_decompile_compile (DO NOT DELETE THIS LINE)
prop_decompile_compile = undefined
-- END prop_decompile_compile (DO NOT DELETE THIS LINE)

-- That concludes this lab!  See the assigment release announcement
-- on the class website for how to submit your lab.

-------------------------------------------------------------------

--                  Utility code

-------------------------------------------------------------------

-- This section contains some utility code for the rest of the lab.
-- You may find it useful if you want to crib some syntax, or if
-- you're just curious what's going on behind the scenes.  Reading
-- this part is OPTIONAL.

-- Our decompiler is implemented as a bit of abstract interpretation:
-- instead of maintaining a stack of integers, we maintain a stack
-- of expressions, and build them up as we execute instructions.

decompile' :: [Instr] -> [Expr] -> Maybe [Expr]
decompile' [] stk = Just stk
decompile' (i:is) stk =
    case decompileInstr i stk of
        Nothing -> Nothing
        Just stk' -> decompile' is stk'

decompileInstr :: Instr -> [Expr] -> Maybe [Expr]
decompileInstr (IPush n) stk = Just (Lit n:stk)
decompileInstr (IOp b) (x:y:rest) = Just (Op b y x:rest)
decompileInstr _ _ = Nothing

-- The next section is devoted to generation of random instructions
-- and expressions.  One interesting thing is that the random
-- instruction generator operates by looking at the stack that would
-- have been produced by the set of instructions generated so far, and
-- only picking an instruction that would be valid in this state.
-- If you are curious about this approach being used on a more
-- interesting language, you may want to check out the paper
-- "Testing Noninterference Quickly."
--
-- Something interesting about this code is that it is monadic.  We
-- will be covering monads later in the quarter; you may find it
-- interesting to revisit this code then.

arbInstr :: Stack -> Gen Instr
arbInstr (_:_:_) = oneof [IOp `fmap` arbitrary, IPush `fmap` choose (0,maxint)]
arbInstr _ = IPush `fmap` choose (0,maxint)

arbInstrs :: Int -> Stack -> [Instr] -> Gen [Instr]
arbInstrs 0 [] rs = return (reverse rs)
arbInstrs 0 stk rs = do
    r <- IOp `fmap` arbitrary
    case step r stk of
        Nothing -> return (reverse rs)
        Just stk' -> arbInstrs 0 stk' (r:rs)
arbInstrs n stk rs = do
    r <- arbInstr stk
    case step r stk of
        Nothing -> return rs
        Just stk' -> arbInstrs (n-1) stk' (r:rs)

#if __GLASGOW_HASKELL__ >= 708
instance {-# OVERLAPPING #-} Arbitrary [Instr] where
#else
instance Arbitrary [Instr] where
#endif
    arbitrary = sized $ \n -> do
                    k <- choose (1,n)
                    arbInstrs k [] []

instance Arbitrary Expr where
    arbitrary = sized $ \n ->
                    if n == 0
                        then Lit `fmap` choose (1,maxint)
                        else resize (n `div` 2)
                            (frequency [(3, binop), (1, Lit `fmap` choose (0,maxint))])
          where binop = do
                    b <- arbitrary
                    e1 <- arbitrary
                    e2 <- arbitrary
                    return (Op b e1 e2)

    shrink = expr_shrink

instance Arbitrary BinOp where
    arbitrary = oneof (map return [Plus, Minus, Times])

instance Arbitrary Instr where
    arbitrary = oneof ([IOp `fmap` arbitrary, IPush `fmap` choose (0,maxint)])

-- We bound the integers generated from 0-10, just to make certain
-- inputs more likely.

maxint :: Int
maxint = 10

-- This is just like 'quickCheck', but testing more cases by default.

check :: Testable prop => prop -> IO ()
check = quickCheckWith stdArgs {maxSuccess = 1000}
