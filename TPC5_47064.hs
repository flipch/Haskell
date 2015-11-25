-- | Felipe Heliszkowski, 47064


module TP5_47064
(  ListaFavoritos
, vazia
,{-- adicionar
, marcar
, extrair--}
) where

import Data.List

data ListaFavoritos a = Nil | Cons {listHead :: a, listTail :: ListaFavoritos a}
                                deriving (Eq, Ord)   

instance (Show a) => Show (ListaFavoritos a) where 
    show (Nil) = "{}"
    show (Cons listHead listTail) = show {'
                                    ++ show listHead 
                                    ++ show ',  
                                    ++ show listTail                                                                                                                         

vazia :: ListaFavoritos a 
vazia = Nil 

adicionar :: Eq a => a -> ListaFavoritos a -> ListaFavoritos a
adicionar = Cons
            
{--
marcar :: Eq a => a -> ListaFavoritos a -> ListaFavoritos a
marcar a = return ()

extrair :: ListaFavoritos a -> ListaFavoritos a
extrair Empty = []
extrair (Cons x xs) 
                | x isFavorito = x : extrair xs
                | otherwise = extrair xs
--}
--isFavorito (Elemento{element = _, favorito = x}) = x


