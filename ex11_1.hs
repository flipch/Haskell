countP :: [a] -> (a -> Bool) -> Int
countP list func = length (filter func list)

sumGTi :: (Ord a, Num a) => [a] -> a
sumGTi list = sum [ x | x <- list, x > 0 ]

sumGTii :: (Ord a, Num a) => [a] -> a
sumGTii list = sum $ filter (>0) list

sumGTiii :: (Ord a, Num a) => [a] -> a
sumGTiii list = foldr (+) 0 (filter (>0) list)
