module Main where

import System.Random
main :: IO ()
main = do
  g <- getStdGen
  let b = foldr inserir vazio (take 60 (randomRs (-500,500) g)) :: Conjunto Integer
  let e = prettyprint b
  putStrLn e
  return ()


data Conjunto a = Vazio | No (Conjunto a) a (Conjunto a) deriving Show

vazio :: Conjunto a
vazio = Vazio

prettyprint Vazio
    = "Empty root."
-- unlines concats a list with newlines
prettyprint (No left node right) = unlines (prettyprint_helper (No left node right))

prettyprint_helper (No left node right)
    = (show node) : (prettyprint_subtree left right)
        where
            prettyprint_subtree left right =
                ((pad "+- " "|  ") (prettyprint_helper right))
                    ++ ((pad "`- " "   ") (prettyprint_helper left))
            pad first rest = zipWith (++) (first : repeat rest)
prettyprint_helper (Vazio)
    = []

singular :: a -> Conjunto a
singular x = No Vazio x Vazio

inserir ::(Ord a) => a -> Conjunto a -> Conjunto a
inserir x Vazio = No Vazio x Vazio
inserir x tree@(No left root right)
                | x < root = No (inserir x left) root right
                | x > root = No left root (inserir x right)
                | otherwise = No left root right

fold :: b -> ( b -> a -> b -> b ) -> Conjunto a -> b
fold x _ Vazio       = x
fold x f (No e y d ) = f ( fold x f e ) y ( fold x f d )

elementos ::(Ord a) => Conjunto a -> [a]
elementos Vazio = []
elementos tree@(No left root right) = elementos left ++ root : elementos right

{-
Como utilizar fold para fazer elementos
-}
