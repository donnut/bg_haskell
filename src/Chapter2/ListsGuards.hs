module ListGuards where

(+++) :: [a] -> [a] -> [a]
[] +++ ys   = ys
(x:xs) +++ ys = x:(xs +++ ys)

head' :: [a] -> a
head' (x:_) = x
head' [] = error "Lijst is leeg"

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:r@(y:_)) = x < y && sorted r

maxmin :: Ord a => [a] -> (a,a)
maxmin [] = error "max or min of empty list is pointless"
maxmin [x] = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

fibonacci' :: Integer -> Maybe Integer
fibonacci' n = if n < 0 then Nothing
                        else case n of
                            0 -> Just 0
                            1 -> Just 1
                            m -> Just (f1 + f2)
                                 where Just f1 = fibonacci' (m-1)
                                       Just f2 = fibonacci' (m-2)


binom :: Num a => Eq a => a -> a -> a
binom _ 0 = 1
binom n k | n == k = 1
binom n k = binom (n-1) (k-1) + binom (n-1) k
