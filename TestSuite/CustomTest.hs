{-# LANGUAGE DeriveGeneric #-}

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Generic)

instance (Out a) => Out (Tree a) where
  doc (Leaf a) =  parens $ text "customLeaf" <+> doc a
  doc (Node a b) = parens $ text "customNode" $$ nest 1 (doc a) 
                                              $$ nest 1 (doc b)
  docPrec _ = doc

tree1 :: Tree Int
tree1 = Node (Node (Leaf 333333) (Leaf (-555555)))(Node (Node(Node(Leaf 888888) 
		(Leaf 57575757))(Leaf (-14141414)))(Leaf 7777777))
			
main = pp tree1