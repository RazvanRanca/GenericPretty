{-# LANGUAGE DeriveGeneric #-}

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Generic)

instance (Out a) => Out (Tree a)

tree1 :: Tree Int
tree1 = Node (Node (Leaf 333333) (Leaf (-555555)))(Node (Node(Node(Leaf 888888) 
		(Leaf 57575757))(Leaf (-14141414)))(Leaf 7777777))
			
zigStyle :: Style
zigStyle = Style {mode = ZigZagMode, lineLength = 30, ribbonsPerLine = 1.5}

main = ppStyle zigStyle tree1