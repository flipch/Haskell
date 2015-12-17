-- Felipe Heliszkowski, 47064
import System.IO
import System.Environment
import ListaFavoritos

main :: IO()
main = do
   args <- getArgs
   let txt = head args
   handle <- openFile txt ReadMode
   contents <- hGetContents handle
   let dados = lines contents
   let marcados = removeNormais dados
   let list = removeMarcas dados
  --putStrLn $ "Dados lidos do ficheiro: " ++ contents
  --putStrLn $ "Lista em Array: " ++ show dados
  --putStrLn $ "Lista total: " ++ show list
  --putStrLn $ "Lista marcados: " ++show marcados
   let lista = foldr adicionar vazia list
  -- putStrLn $ "Lista de Favoritos desmarcada: " ++ show lista
   let lista_marcada = marcarLista marcados lista
   --putStrLn $ "Lista de Favoritos marcada: " ++ show lista_marcada
   print $ extrair lista_marcada
   return ()

marcarLista :: (Eq a, Foldable t) =>
                 t a -> ListaFavoritos a -> ListaFavoritos a
marcarLista xs list = foldr marcar list xs

removeMarcas :: [String] -> [String]
removeMarcas [] = []
removeMarcas (x:xs)
              | isMarked x = init x : removeMarcas xs
              | otherwise = x : removeMarcas xs

removeNormais :: [String] -> [String]
removeNormais [] = []
removeNormais (x:xs)
              | isMarked x = init x : removeNormais xs
              | otherwise = removeNormais xs

isMarked :: String -> Bool
isMarked x
    | last x == '*' = True
    | otherwise     = False
