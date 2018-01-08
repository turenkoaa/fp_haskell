import Prelude hiding (foldr, foldl)

foldr :: (t -> t1 -> t1) -> t1 -> [t] -> t1
foldl :: (t1 -> t -> t1) -> t1 -> [t] -> t1

foldr _ ini [] = ini
foldr f ini (x:xs) = x `f` foldr f ini xs

foldl _ ini [] = ini
foldl f ini (x:xs) = foldl f (ini `f` x) xs

map' f xs = foldr (\a as -> (f a) : as) [] xs
flatMap' f xs = foldr (\a as -> (f a) ++ as) [] xs
concat' xs = foldr (++) [] xs
filter' f xs = foldr (\a as -> if (f a) then a : as else as) [] xs
maxBy' f xs = foldr (\a b -> max (f a) b) (f (last xs)) xs
minBy' f xs = foldr (\a b -> min (f a) b) (f (last xs)) xs
reverse' xs = foldr (\a as -> as ++ [a]) [] xs
elementAt' index list@(x:xs) | index <= 0 || index >= length list = error "out of index"
                             | otherwise = value
                             where (_, value) = foldl (\p@(i, v) a -> if (i > index) then p else (i + 1, a)) (0, x) list
indexOf' value list = index where
  (index, _) = foldl (\(i1, i2) v -> if v == value && i1 == (-1) then (i2, i2+1) else (i1, i2+1)) (-1, 0) list
