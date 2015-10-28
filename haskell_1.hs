



combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..length xs-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p (x:xs) | p x       = x : xs'
                 | otherwise = xs'
                  where xs'  = filter2 p xs
filter2 _ []                 = []

allCombinations _ 0 = [[]]
allCombinations xs count = combinations count xs ++ allCombinations xs (count-1)

sum' [] = 0
sum' [(x,y)] = y
sum' ((a,b):xs) = b + sum' xs

isSomaNula xs = sum' xs == 0


somasNulas [] = []
somasNulas xs = somasNulas' (allCombinations xs (length xs))
              where somasNulas' [] = []
                    somasNulas' (x:xs)
                                      | isSomaNula x = x: somasNulas' xs
                                      | otherwise = somasNulas' xs

{- somasNulas xs = [ x | i <- [0..length xs-1]
                    ,  x <- (allCombinations xs (length xs))
                    ,  sum' x 0 == [0]  ]
            --  where b = filter2 isSomaNula (allCombinations xs (length xs))

--somasNulas xs = [ x | x <- combinations (length xs) xs, filter' (sum x == 0) x ]
--somasNulas xs = [ xs !! i : x | i <- [0..length xs-1]
--                              , x <- somasNulas (drop (i+1) xs)]
-}
