data Shape = Circle Float | Rectangle Float Float deriving (Show)

instance Eq Shape where
  Circle raio1 == Circle raio2 = True
  Rectangle a1 c1 == Rectangle a2 c2 = True
  _ == _ = False

instance Ord Shape where
  compare (Circle r1) (Circle r2)
      | r1 > r2 = GT
      | r1 < r2 = LT
      | otherwise = EQ
  compare (Circle r1) (Rectangle a c)
      | r1 > (a*c) = GT
      | r1 < (a*c) = LT
      | otherwise = EQ
  compare (Rectangle a c) (Circle r1)
      | (a*c) > r1 = GT
      | (a*c) < r1 = LT
      | otherwise = EQ
  compare (Rectangle a1 c1) (Rectangle a2 c2)
      | (a1*c1) > (a2*c2) = GT
      | (a1*c1) < (a2*c2) = LT
      | otherwise = EQ

data ShapeTree = Empty | Node ShapeTree Shape ShapeTree deriving (Show)

insertShape :: Shape -> ShapeTree -> ShapeTree
insertShape shape (Empty) = Node Empty shape Empty
insertShape shape tree@(Node left value right)
      | shape < value = Node (insertShape shape left) value right
      | shape > value = Node left value (insertShape shape right)
      | otherwise = tree

writeShapes :: ShapeTree -> String -> IO()
writeShapes tree filename= do
  writeFile filename (printTree tree)
  return ()

printTree :: ShapeTree -> String
printTree tree@(Node left val right) = printTree right ++ "\n" ++ show val ++ printTree left
printTree Empty = ""
