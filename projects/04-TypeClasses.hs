module TypeClasses where

import Data.List (sortBy)

-- In this lab, we will investigate explicit dictionary passing
-- style for the Ord typeclass, and in this setting investigate the
-- consequences of an extended form of type classes where instances can
-- be defined locally.  The answers in this section should be very
-- short.  You will probably find it quite helpful to load up this
-- lab in GHCi and ask for the types of various operators.

-- In this lab, we'll assume the Ord type class is defined as follows:
--
--      data Ordering = LT | EQ | GT
--      class Ord a where
--          compare :: a -> a -> Ordering
--
-- (The real Ord type class is a bit more complicated, but we'll use
-- this approximation for the lab at hand.)
--
-- With this definition, we can write down the definition for a
-- dictionary as follows:

data OrdD a = OrdD { compareD :: a -> a -> Ordering }

-- To avoid confusion between the dictionary world and the type classes
-- world, we'll append 'D' to all identifiers in the dictionary world
-- to disambiguate them:
--
--    Type classes          Dictionaries
--    =========================================
--    TypeClass (Ord)       TypeClassD (OrdD)
--    method (compare)      methodD (compareD)
--    function (sort)       functionD (sortD)
--
-- Recall that the FULL type of the compareD selector is:
--
--      compareD :: Ord a -> a -> a -> Ordering
--
-- Int has a built-in Ord instance, using some built-in comparison
-- primitives.
--
--      instance Ord Int where
--          ...
--
-- We've provided the dictionary for it here (instances are unnamed
-- in the type class world, we'll give them names prefixed with d
-- in the dictionary world):

dOrdInt :: OrdD Int
dOrdInt = OrdD { compareD = Prelude.compare }

-- We can also define a sorting function which uses our dictionary,
-- using the 'sortBy :: (a -> a -> Bool) -> [a] -> [a]' function
-- provided by the standard library.

sortD :: OrdD a -> [a] -> [a]
sortD d xs = sortBy (compareD d) xs

-- Here's a list of Ints:

exIntList :: [Int]
exIntList = [2,3,1]

-- In standard Haskell, you would just write:
--
--      sort exIntList
--
-- Translate this expression to dictionary passing style.

exSortInt :: [Int]
-- BEGIN exSortInt (DO NOT DELETE THIS LINE)
exSortInt = undefined
-- END exSortInt (DO NOT DELETE THIS LINE)

-- Pairs (a, b) also have an Ord instance. It looks like this:
--
--      instance (Ord a, Ord b) => Ord (a, b) where
--          compare (a1, b1) (a2, b2) =
--              case compare a1 a2 of
--                  EQ -> compare b1 b2
--                  r -> r
--
-- i.e., it sorts coordinates from x to y.
-- Please translate this instance into a dictionary.
--
-- Note: Haskell's record syntax doesn't support defining
-- a function directly in a record; if you want to define a
-- record with a lambda, you should use this syntax:
--
--      OrdD { compare = \(a1, b1) (a2, b2) -> ... }

dOrdXY :: OrdD a -> OrdD b -> OrdD (a, b)
-- BEGIN dOrdXY (DO NOT DELETE THIS LINE)
dOrdXY = undefined
-- END dOrdXY (DO NOT DELETE THIS LINE)

-- Here's a list of pairs of Ints:

exIntPairList :: [(Int, Int)]
exIntPairList = [(2,1), (1,2)]

-- In standard Haskell, you would just write:
--
--      sort exIntPairList
--
-- Translate this to dictionary passing style.

exSortIntPair :: [(Int, Int)]
-- BEGIN exSortIntPair (DO NOT DELETE THIS LINE)
exSortIntPair = undefined
-- END exSortIntPair (DO NOT DELETE THIS LINE)

-- The previous ordering instance we gave is a bit arbitrary;
-- we could have instead defined it to sort by y-coordinate.
-- Write the dictionary for this version here (it should look
-- very similar to dOrdXY):

dOrdYX :: OrdD a -> OrdD b -> OrdD (a, b)
-- BEGIN dOrdYX (DO NOT DELETE THIS LINE)
dOrdYX = undefined
-- END dOrdYX (DO NOT DELETE THIS LINE)

-- With type-classes, it is not possible to define both of these
-- instances simultaneously.  We can see why if we look at some code
-- which wants to sort points:
--
--      sort [(1,2), (2,1)]
--
-- When we translate to dictionary style, do we use dOrdXY or dOrdYX?
-- Type-class resolution is TYPE directed, and since these dictionaries
-- both apply to the same types, we can't tell if we want one or the
-- other.
--
-- The problem is that when I define an instance in Haskell, it becomes
-- "globally" available: anywhere I have a type class constraint, I am
-- allowed to use the instance.  Unfortunately, with Ord it is a common
-- occurrence to want to use alternate instances for objects. Here is
-- another example: the default sorting order for integers is ascending;
-- however, you might also like to sort integers descending.  Here's an
-- alternate dictionary which reverses the sense of comparison by
-- negating the arguments to compare:

dOrdIntDesc :: OrdD Int
dOrdIntDesc = OrdD $ \x y -> Prelude.compare (-x) (-y)

-- ... but you can't define an instance for both ascending and
-- descending comparison at the same time, for the very same reason.

-- An often proposed extension to type classes is to allow them to
-- be defined locally.  Here is some example syntax:
--
--      exSortIntDesc :: [Int]
--      exSortIntDesc =
--          let instance Ord Int where
--                  compare x y = Prelude.compare y x
--          in sort exIntList
--
-- This code is not legal Haskell, but see the end of this lab for
-- some example Scala code which is morally equivalent to this
-- pseudocode.
--
-- When run, this code produces the reverse-sorted exIntList.  Translate
-- this into dictionary passing style (it should look very familiar!):

exSortIntDesc :: [Int]
-- BEGIN exSortIntDesc (DO NOT DELETE THIS LINE)
exSortIntDesc = undefined
-- END exSortIntDesc (DO NOT DELETE THIS LINE)

-- Local instance declarations don't have any effect on runtime,
-- besides influencing what DICTIONARY is chosen to be filled in.

-- With local instances, we can even specialize the sort function:
--
--      sortIntDesc :: [Int] -> [Int]
--      sortIntDesc xs =
--          let instance Ord Int where
--                  compare x y = Prelude.compare (-x) (-y)
--          in sort xs
--
-- Translate this to dictionary passing style, using your preexisting
-- 'sort' function.  (This is simple, not a trick question!) (Hint:
-- first write down what the TYPE of the converted function should
-- be, and then fill in the implementation.)

-- BEGIN sortIntDesc (DO NOT DELETE THIS LINE)
sortIntDesc :: your_type_signature_here
sortIntDesc = undefined
-- END sortIntDesc (DO NOT DELETE THIS LINE)

-- However, local instances can give rise to some odd behavior.  For
-- example, here is another valid, "more general" type signature for
-- sortIntDesc:
--
--      sortIntDesc' :: Ord a => [a] -> [a]
--      sortIntDesc' xs =
--          let instance Ord Int where
--                  compare x y = Prelude.compare (-x) (-y)
--          in sort xs
--
-- This code does in fact typecheck, but it does something
-- different from sortIntDesc.  Please translate it to dictionary
-- passing style.  (Hint: first write down what the TYPE of the
-- converted function should be, and then fill in the implementation.
-- Remember that class constraints get translated into dictionary
-- arguments.)  (Hint 2: you can't pattern match on a function
-- argument in Haskell.)  (Hint 3: Without any extra constraints, there
-- is no way to type-case on the type of a variable.)

-- BEGIN sortIntDesc' (DO NOT DELETE THIS LINE)
sortIntDesc' :: your_type_signature_here
sortIntDesc' = undefined
-- END sortIntDesc' (DO NOT DELETE THIS LINE)

-- In fact, with local instances, we lose "principal types", an
-- observation that was first made in Wadler and Blott.  The chosen
-- type of an expression can change the meaning of the expression,
-- which means that there is no most general choice: the choices
-- are incompatible and do different things.  This means we cannot
-- rely inference: with local instances, we are, in many cases, REQUIRED
-- to give explicit type signatures.

-- Here is another instance of strange behavior.
-- In Haskell, it's very common to specify an API which uses type class
-- instances to manage some internal invariant.  For example, here is
-- a function on the Map data type:
--
--      Map.insert :: Ord k => k -> a -> Map k a -> Map k a
--
-- Map uses the Ord instance in order to enforce the internal ordering
-- invariant of keys in the tree.
--
-- Explain, in a few plain-English sentences, what problems can arise
-- for this kind of code when local instances are allowed.

{-
-- BEGIN localInstancesInvariant (DO NOT DELETE THIS LINE)
(your English here)
-- END localInstancesInvariant (DO NOT DELETE THIS LINE)
-}

{-
-- There is nothing left in the lab, but we promised you some
-- Scala code that uses local instances.  Here it is:

object HelloWorld {
  trait Ord[A] {
    def lte(x: A, y: A): Boolean
  }
  implicit object OrdInt extends Ord[Int] {
    def lte(x: Int, y: Int): Boolean = x <= y
  }
  def sort[T](list: List[T])(implicit ord: Ord[T]): List[T] = list.sortWith(ord.lte)
  def intsort(list: List[Int])(implicit ord: Ord[Int]): List[Int] = {
    implicit object RevOrdInt extends Ord[Int] {
      def lte(x: Int, y: Int): Boolean = -x <= -y
    }
    sort(list)
  }
  def intsort2[T](list: List[T])(implicit ord: Ord[T]): List[T] = {
    implicit object RevOrdInt extends Ord[Int] {
      def lte(x: Int, y: Int): Boolean = -x <= -y
    }
    sort(list)
  }
  def main(args: Array[String]) {
    val exList: List[Int] = List(4,3,1,5,2,6,0);
    println(intsort2(exList));
  }
}

-- This code prints out List(0, 1, 2, 3, 4, 5, 6)
-}
