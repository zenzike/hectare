module Hectare.Util where

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave xs ys = head xs : head ys : interleave (tail xs) (tail ys)

pairSwap :: [([a],[a])] -> [([a],[a])]
pairSwap [] = []
pairSwap [x] = [x]
pairSwap (x:(s,t):xs) = x : (reverse s,reverse t) : pairSwap xs

concatBash :: [[a]] -> [a]
concatBash [] = []
concatBash xs = (head . head) xs : concatMap tail xs

boustrophedron :: [[a]] -> [a]
boustrophedron [] = []
boustrophedron [x] = x
boustrophedron (x:y:xs) = tail x ++ (tail . reverse) y ++ boustrophedron xs
-- boustrophedron (x:y:xs) = x ++ [last x,last x] ++ (reverse y) ++ [head y] ++ boustrophedron xs

bundle :: Int -> [a] -> [[a]]
bundle _ [] = []
bundle n xs = ys : bundle n yss
  where
    (ys, yss) = splitAt n xs

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

(×) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f × g) (x, y) = (f x, g y)

