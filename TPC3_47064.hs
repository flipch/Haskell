-- 47064, Felipe H

maiorOrdenado a@(c@(y,_,_,x,_):xs)
                  | x == b = y
                  | otherwise = maiorOrdenado xs
                  where b = maiorOrdenado' a

maiorOrdenado' [] = 0
maiorOrdenado' ((_,_,_,x,_):xs)
                  | x >= maiorOrdenado' xs = x
                  | otherwise = maiorOrdenado' xs

maiorDepartamento xs = map (\(x,a,y,b,c) -> (x,y))
                          (maiorNumero [(count c xs, c) | c <- rmdups xs])


maiorNumero a@((y,x):xs)
                | y == b = x
                | otherwise = maiorNumero xs
                where b = maiorNumero' a

maiorNumero' [] = 0
maiorNumero' ((x,_):xs)
                | x >= maiorNumero' xs = x
                | otherwise = maiorNumero' xs

ordenadoMedio xs = ordenadoMedio' ys
            where ys = map (\(x,a,y,b,c) -> (y,b))

ordenadoMedio' xs = [ (x, sumOrds x xs `quot`count x xs) | x <- rmdups xs]

sumOrds x [] = []
sumOrds x (y:ys) | fst y == x = snd y + sumOrds x ys
                 | otherwise = sumOrds  ys

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
