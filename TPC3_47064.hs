-- 47064, Felipe H

maiorOrdenado a@(c@(y,_,_,x,_):xs)
                  | x == b = y
                  | otherwise = maiorOrdenado xs
                  where b = maiorOrdenado' a

maiorOrdenado' [] = 0
maiorOrdenado' ((_,_,_,x,_):xs)
                  | x >= maiorOrdenado' xs = x
                  | otherwise = maiorOrdenado' xs

maiorDepartamento xs = maiorDepartamento' (map (\(x,a,y,b,c) -> (x,y)) xs)

maiorDepartamento' xs
                    = maiorNumero [(count c xs, c) | c <- rmdups xs]

maiorNumero a@((y,x):xs)
                | y == b = x
                | otherwise = maiorNumero xs
                where b = maiorNumero' a

maiorNumero' [] = 0
maiorNumero' ((x,_):xs)
                | x >= maiorNumero' xs = x
                | otherwise = maiorNumero' xs

count x [] = 0
count x ((_,y):ys)
      | x == y = 1 + count x ys
      | otherwise = count x ys

rmdups xs = rmdups' (getSnd xs)

rmdups' [] = []
rmdups' (x:xs) = x : rmdups' (filter (/= x) xs)

getSnd [] = []
getSnd ((x,y):xs) = y : getSnd xs

getFst [] = []
getFst ((x,y):xs) = x : getFst xs
