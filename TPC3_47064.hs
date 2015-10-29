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

maiorDepartamento' xs = maiorNumero [(count c xs, c) | c <-(rmdups (getSnd xs))]

maiorNumero a@((y,x):xs)
                | y == b = x
                | otherwise = maiorNumero xs
                where b = maiorNumero' a

maiorNumero' [] = 0
maiorNumero' ((x,_):xs)
                | x >= maiorNumero' xs = x
                | otherwise = maiorNumero' xs

ordenadoMedio xs = ordenadoMedio' ys
                where ys = map (\(a,b,c,d,e) -> (c,d)) xs

ordenadoMedio' xs = [ (x, sumOrds x xs `quot`count' x xs) |
                                                    x <- (rmdups (getFst xs))]

sumOrds _ [] = 0
sumOrds x ((a,b):xs)
          | a == x = b + sumOrds x xs
          | otherwise = sumOrds x xs

count' _ [] = 0
count' x ((y,_):ys)
        | x == y = 1 + count' x ys
        | otherwise = count' x ys

count _ [] = 0
count x ((_,y):ys)
        | x == y = 1 + count x ys
        | otherwise = count x ys

rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

getSnd [] = []
getSnd ((x,y):xs) = y : getSnd xs

getFst [] = []
getFst ((x,y):xs) = x : getFst xs
