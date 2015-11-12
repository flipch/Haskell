-- 47064, Felipe Heliszkowski

{-

A função divisoresPares retorna uma lista de todos os Divisores de N que sejam Pares.

A função isFascinante retorna true se n for um numero fascinante, ou false caso contrário.

A função fascinantes retorna todos os numeros fascinantes desde 1 até N.

-}
divisoresPares :: Integral a => a -> [a]
divisoresPares n = [x | x <- [1..(n-1)], n `mod` x == 0, x `mod` 2 == 0]

isFascinante :: Integral a => a -> Bool
isFascinante n = sum (divisoresPares n) == n 

fascinantes :: Integral a => a -> [a]
fascinantes n = [x | x <- [1..(n-1)], isFascinante x]

f::Bool -> Integer
f a = 1

g:: a -> b -> [(b,a)]
g a b = [(b,a)]

h::Ord t => [t] -> t -> Bool
h xs x = True


