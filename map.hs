map' f (x:xs)   = f x : map' f xs
map' _ [] = []

map'' f xs      = foldr (\ x xs -> f x : xs) [] xs
