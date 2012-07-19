{-# LANGUAGE DeriveGeneric #-}

import Text.PrettyPrint.GenericPretty

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Generic)

instance (Out a) => Out (Tree a)

tree1 :: Tree Int
tree1 = Node (Node (Leaf 333333) (Leaf (-555555)))(Node (Node(Node(Leaf 888888) 
		(Leaf 57575757))(Leaf (-14141414)))(Leaf 7777777))
			
--main = pp tree1
main = ppLen 30 tree1