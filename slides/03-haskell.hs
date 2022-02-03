module Basic where

import Debug.Trace (trace)
import qualified Data.List as L


replicat :: Int -> a -> [a]
replicat 0 _ = []
replicat n x = x : replicat (n-1) x

sm :: [Int] -> Int
sm [] = 0
sm (x:xs) = x + sm xs

pr :: [Int] -> Int
pr [] = 1
pr (x:xs) = x * pr xs

fac n = product [1..n]

-- [1,2,3]
-- 1 : (2 : (3 : []))

--      f
--     / \
--    1   f
--       / \
--       2  f
--         / \
--        3   z



-- Lists in Haskell!
--
-- List literals: [1,2,3]
-- Singly linked lists!
--  empty list: []  <~~ constructors
--  cons: (x:xs)
--  ^-- pattern match
-- useful function: append ++
--
--
-- recursion (many occurrences of recursion)
-- higher order functions: recursion PATTERNS
--      map (implemented)
--      filter

-- Hello world!


-- [[1,2],
--  [3,4]]
--
-- [[1,3],
--  [2,4]]
--
-- xss = [[3, 4]]
--
-- xs = [1,2]
-- transp xss = [[3],
--               [4]]
--
-- r = [[1,3],
--      [2,4]]

-- HOW TO RECURSE?
--  what's the base case
--  and what's the recursive case

transp :: [[Int]] -> [[Int]]
transp [] = []
transp (xs:xss) = combine xs (transp xss)

combine :: [Int] -> [[Int]] -> [[Int]]
combine [] _ = []
combine (x:xs) [] = [x] : combine xs []
combine (x:xs) (ys:yss) = (x:ys) : combine xs yss


listAddTwo :: [Int] -> [Int]
{-
listAddTwo [] = []
listAddTwo [x] = [x+2]
listAddTwo [x,y] = [x+2,y+2]
-}
listAddTwo [] = []
listAddTwo (x:xs) = x+2 : ys
    where
        ys = listAddTwo xs

-- divergence:
--  * higher order functions lambdas
--      * what's currying
--  * recursion in complicated situations

-- listMulFour :: [Int] -> [Int]

-- polymorphism!
myMap :: (a -> b) -> [a] -> [b]  -- why is written this way?
myMap f [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

-- arrows associate to the right: a -> (b -> c)
-- spaces associate to the left:  (f a) b
add :: Int -> (Int -> Int)
add x y = x + y

addUncurried :: (Int, Int) -> Int
addUncurried (x, y) = x + y


rev0 :: [a] -> [a]
rev0 [] = []
rev0 (x : xs) = reversed_xs ++ [x]
    where
        reversed_xs = rev0 xs

rev :: [Int] -> [Int]
rev xs = go xs (trace "HELLO" [])
    where
        go [] rs = rs
        go (x:xs) rs = trace ("DEBUG") $ go xs (x:rs)


frozzle :: Int -> Int
frozzle x = (let w = z + 3 in w * w) * 4
    where
        y = 2 + x
        z = 3 * y + x


len :: [a] -> Int
len [] = 0
len (x : xs) =  len xs + 1

-- 5! = 5*4*3*2*1
--
-- function factorial(x) {
-- 	if (x == 0) { return 1; }
-- 	else { return factorial(x-1) * x; }
-- }

factorial :: Integer -> Integer
factorial x =
    case x of
        0 -> 1
        x -> factorial (x - 1) * x

{-
    | x <= 0    = 1
    | otherwise = factorial (x-1) * x
-}

