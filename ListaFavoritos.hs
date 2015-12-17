{-
PRINCÍPIOS DE PROGRAMAÇÃO 2015/2016
Licenciatura em Engenharia Informática

Faculdade de Ciências
Universidade de Lisboa

Uma solução para o trabalho 5

Vasco T. Vasconcelos
Mário Guimarães
$Id: ListaFavoritos.hs 397 2015-12-10 15:23:34Z mlguimaraes $
-}

module ListaFavoritos
( ListaFavoritos
, vazia
, estaVazia
, adicionar
, contem
, marcar
, estaMarcado
, extrair
) where

import Data.List (intercalate)

data ListaFavoritos a = LF [(a, Bool)] deriving Eq

vazia :: ListaFavoritos a
vazia = LF []

adicionar :: Eq a => a -> ListaFavoritos a -> ListaFavoritos a
adicionar e lf@(LF xs) = if contem e lf then lf else LF (xs ++ [(e, False)])

contem :: Eq a => a -> ListaFavoritos a -> Bool
contem e (LF xs) = (e, False) `elem` xs || (e, True) `elem` xs

estaVazia :: ListaFavoritos a -> Bool
estaVazia (LF xs) = null xs

marcar :: Eq a => a -> ListaFavoritos a -> ListaFavoritos a
marcar e (LF xs) =
  LF $ foldr (\p@(f, m) acc -> if e == f then (e, True) : acc else p : acc) [] xs

estaMarcado :: Eq a => a -> ListaFavoritos a -> Bool
estaMarcado e (LF xs) = (e, True) `elem` xs

extrair :: ListaFavoritos a -> ListaFavoritos a
extrair (LF xs) = LF $ map (\(x, _) -> (x, False)) (filter snd xs)

instance Show a => Show (ListaFavoritos a) where
  show (LF xs) = '{' : (intercalate ", " (map itemToString xs)) ++ "}"
    where itemToString (x, False) = show x
          itemToString (x, True)  = show x ++ "*"

umaLista :: ListaFavoritos Char
umaLista = foldr adicionar vazia "bacaaced"
