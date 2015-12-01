-- | Felipe Heliszkowski, 47064

module TP5_47064
( ListaFavoritos
, vazia
, adicionar
, marcar
, extrair
) where


data ListaFavoritos a = Nil | Cons { listTail :: ListaFavoritos a, listHead :: Item a}
  deriving (Eq, Ord)

instance (Show a) => Show (ListaFavoritos a) where
      show Nil = "{}"
      show x = "{" ++ toString x ++ "}"

data Item a = Single {value :: a, fav :: Bool} deriving (Eq, Ord)

instance (Show a) => Show (Item a) where
    show (Single value fav) | fav = show value ++ "*"
                            | otherwise = show value

toString :: Show a => ListaFavoritos a -> String
toString (Cons Nil element) = show element
toString (Cons list element) = toString list ++ "," ++ show element

vazia :: ListaFavoritos a
vazia = Nil

adicionar :: Eq a => a -> ListaFavoritos a -> ListaFavoritos a
adicionar x y | contains x y = y
              | otherwise = Cons y (Single x False)

contains :: Eq a => a -> ListaFavoritos a -> Bool
contains _ Nil = False
contains x (Cons ys list) | Single x False == list = True
                          | otherwise = contains x ys

marcar :: Eq a => a -> ListaFavoritos a -> ListaFavoritos a
marcar x y | contains x y = popAndMarcar x y Nil
           | otherwise = y -- Do nothing if it isnt in the list

popAndMarcar :: Eq a => a -> ListaFavoritos a -> ListaFavoritos a -> ListaFavoritos a
popAndMarcar _ Nil newList = newList
popAndMarcar x (Cons ys y@(Single val _)) newList
            | x == val = popAndMarcar x ys (Cons newList (Single x True))
            | otherwise = popAndMarcar x ys (Cons newList y)

extrair :: ListaFavoritos a -> ListaFavoritos a
extrair Nil = Nil
extrair (Cons Nil x@(Single _ fav)) = if fav
  then Cons Nil x
  else Nil
extrair (Cons y x@(Single _ fav))
                          | fav = Cons (extrair y) x
                          | otherwise = extrair y
