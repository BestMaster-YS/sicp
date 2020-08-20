a = [1..]

merge :: Num a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = (x+y) : merge xs ys

merge1 :: Num a => [a] -> [a] -> [a]
merge1 xs [] = xs
merge1 [] ys = ys
merge1 (x:xs) ys = x : merge1 (merge xs ys) ys


factorial xs  = merge1 xs xs


