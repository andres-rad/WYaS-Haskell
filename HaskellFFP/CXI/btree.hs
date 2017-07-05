data BinaryTree a = Leaf
				  | Node (BinaryTree a) a (BinaryTree a)
				  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
		| b == a = Node left a right
		| b < a  = Node (insert' b left) a right
		| otherwise = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left a right) = (Node (mapTree f left) (f a) (mapTree f right))

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected
		  then print "yup okay!"
		  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = a : preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = inorder l ++ [a] ++ inorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a] 

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
			   then putStrLn "Preorder fine!"
			   else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
	if inorder testTree == [1, 2, 3]
	then putStrLn "Inorder fine!"
	else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
	if postorder testTree == [1, 3, 2]
	then putStrLn "Postorder fine!"
	else putStrLn "postorder failed check"

main :: IO ()
main = do
	testPreorder
	testInorder
	testPostorder


foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node l a r) = f a (foldTree f acc l) (foldTree f acc r)



mapTreePre :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTreePre f = foldTree (\a l r -> (Node l (f a) r) ) Leaf 