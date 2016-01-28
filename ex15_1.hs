data Entry = Entry String String deriving Show

instance Eq Entry where
  Entry k1 _ == Entry k2 _ = k1 == k2

instance Ord Entry where
  compare (Entry k1 _) (Entry k2 _) =  compare k1 k2

printEntry :: Entry -> String
printEntry (Entry key value) = "Chave: " ++ key ++ "\nValue: " ++ value

saveEntries :: [Entry] -> IO()
saveEntries [] = return ()
saveEntries (x:xs) = do
    saveEntry x
    saveEntries xs
    return ()

saveEntry :: Entry -> IO()
saveEntry entry@(Entry key value) = do
  putStrLn $ "Deseja gravar " ++ key ++ " ?"
  awnser <- getLine
  if awnser == "s"
    then appendFile "Dictionary.txt" $ printEntry entry ++ "\n"
    else return ()
