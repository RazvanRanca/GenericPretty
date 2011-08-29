{-# LANGUAGE DeriveGeneric #-}

{- Tests.hs has a number of different custom data types. All of them implement 'Out' and 'Arbitrary'.
Properties are provided for each that specify that the output given by 'pretty' and that given
by 'show' should be identical except for the whitespace 

The different data types follow the same pattern of implementation and functions, so most of the
functions are only commented for the first data type -}

import Text.PrettyPrint.GenericPretty
import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Text.Printf

-- used to make pretty printed and show output identical for comparing purposes
removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs)
  | x `elem` " \n" = removeSpaces xs
  | otherwise = x : removeSpaces xs
  
-- checks the output of a specific value
checkOutput :: (Out a, Show a) => a -> Bool
checkOutput a = removeSpaces (pretty a) == removeSpaces (show a)

-- Finite State Machine Type
data FSM q = FSMCons ([q], Alphabet, q, [q], [Transition q]) deriving (Show, Generic)
type Alphabet = String
type Transition q = (q, Char, q)

-- implement 'Out' so we can pretty print
instance (Out a) => Out (FSM a)

--implementation needed for quickCheck generation of random values
instance Arbitrary a => Arbitrary (FSM a) where
	arbitrary = liftM FSMCons arbitrary

-- check wether 'Maybe Int' FSM's are outputed the same via pretty and show (modulo the whitespace) 
checkFSM :: FSM (Maybe Int) -> FSM (Maybe Int) -> Bool
checkFSM _ a = removeSpaces (pretty a) == removeSpaces (show a)

-- example of an FSM, you can check the output of this manually with 'checkOutput'
f :: FSM Int
f = FSMCons([0,1,2,3,4],
      "ab",
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])
	   
-- Binary Tree data type
data BinaryTree a = EmptyBTree | BNode a (BinaryTree a) (BinaryTree a) deriving (Show, Generic)  

instance (Out a) => Out (BinaryTree a)

instance (Arbitrary a) => Arbitrary (BinaryTree a) where
	arbitrary = sized arbitTree
		where 
			arbitTree 0 = return EmptyBTree
			arbitTree n 
				| n>0 = oneof [return EmptyBTree, liftM3 BNode arbitrary subTree subTree]
				| otherwise = error "tree size should never be < 0"
				where
					subTree = arbitTree (n `div` 2)

checkBinaryTree :: BinaryTree Char -> BinaryTree Char -> Bool
checkBinaryTree _ a = removeSpaces (pretty a) == removeSpaces (show a)

-- functions for the construction of BinaryTrees					
singleton :: a -> BinaryTree a  
singleton x = BNode x EmptyBTree EmptyBTree  
      
treeInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a  
treeInsert x EmptyBTree = singleton x  
treeInsert x (BNode a left right)   
    | x == a = BNode x left right  
    | x < a  = BNode a (treeInsert x left) right  
    | x > a  = BNode a left (treeInsert x right)  

nums :: [Int]
nums = [55555,99999,22222,77777,88888,11111,33333,44444,66666]  

-- mkBTree takes a list and creates a BinaryTree docPrec of it
mkBTree :: (Ord a) => [a] -> BinaryTree a
mkBTree = foldr treeInsert EmptyBTree

-- example BinaryTree
bt :: BinaryTree Int
bt = mkBTree nums

-- Tree using record syntax
data RecordTree a = RNode {val :: a, children :: [RecordTree a]} deriving (Show, Generic)  

instance (Out a) => Out (RecordTree a)

instance (Arbitrary a) => Arbitrary (RecordTree a) where
	arbitrary = sized arbitTree
		where
			arbitTree 0 = liftM2 RNode arbitrary (return [])
			arbitTree n 
				| n>0 = liftM2 RNode arbitrary childList
				| otherwise = error "tree size should never be < 0"
				where
					childList = resize (floor.sqrt.fromIntegral $ n) (listOf (arbitTree (n`div` 2)) )

checkRecordTree :: RecordTree String -> RecordTree String -> Bool
checkRecordTree _ a = removeSpaces (pretty a) == removeSpaces (show a)
					
rt :: RecordTree Int
rt = RNode (-656565) [RNode 33344 [], RNode 98789 [RNode (-766444) [], RNode 454545 [], RNode 59996 []]]

infixr 5 :*:
infixr 3 :+:
-- tree using infix notation
data InfixTree a = ILeaf a a | (InfixTree a) :*: (InfixTree a) | (InfixTree a) :+: (InfixTree a) 
		deriving (Show, Generic)  
		
instance (Out a) => Out (InfixTree a)

instance (Arbitrary a) => Arbitrary (InfixTree a) where
	arbitrary = sized arbitTree
		where
			arbitTree 0 = liftM2 ILeaf arbitrary arbitrary
			arbitTree n 
				| n>0 = oneof [ liftM2 (:*:) subTree subTree, liftM2 (:+:) subTree subTree]
				| otherwise = error "tree size should never be < 0"
				where
					subTree = arbitTree (n `div` 2)

nt :: InfixTree Int
nt = ILeaf 5454544 55 :*: (ILeaf 5375738 44 :+: ((ILeaf 699879 55 :*: ILeaf 2332323 66 :+: ILeaf 676765 77) 
		:*: ILeaf 99999 88) ) :+: ILeaf 555 666

checkInfixTree :: InfixTree (Either Int Char) -> InfixTree (Either Int Char) -> Bool
checkInfixTree _ a = removeSpaces (pretty a) == removeSpaces (show a)
	
infixr 5 :^:
-- infix and record tree, also uses a second user defined type in it's definition, 'Wrap'
data InfixRecordTree a = IRLeaf (Wrap a) | (:^:) {left :: InfixRecordTree a, right :: InfixRecordTree a} 
			deriving (Show, Generic)  
			
instance (Out a) => Out (InfixRecordTree a)
	
instance (Arbitrary a) => Arbitrary (InfixRecordTree a) where
	arbitrary = sized arbitTree
	  where
		arbitTree 0 = liftM IRLeaf arbitrary
		arbitTree n
			| n>0 = liftM2 (:^:) subTree subTree
			| otherwise = error "tree size should never be < 0"
			  where
				subTree = arbitTree (n `div` 2)
				
irt :: InfixRecordTree Int
irt = IRLeaf  (Wrap 5454544) :^: (IRLeaf (Wrap (-5375738)) :^: ((IRLeaf  (Wrap 699879) :^: 
		(IRLeaf (Wrap (-2332323)) :^: IRLeaf (Wrap 676765))) :^: IRLeaf (Wrap 99999)))

checkInfixRecordTree :: InfixRecordTree Float -> InfixRecordTree Float -> Bool
checkInfixRecordTree _ a = removeSpaces (pretty a) == removeSpaces (show a)
	
-- just a very simple user defined type that is used in IRTree
data Wrap a = Wrap a deriving (Show, Generic)

instance Out a => Out (Wrap a)
	
instance Arbitrary a => Arbitrary (Wrap a) where
	arbitrary = liftM Wrap arbitrary
	
allTests = [	("FSM (Maybe Int)", quickCheck checkFSM),
				("BinaryTree (Char)", quickCheck checkBinaryTree),
				("RecordTree (String)", quickCheck checkRecordTree),
				("InfixTree (Either Int Char)", quickCheck checkInfixTree),
				("InfixRecordTree (Float)", quickCheck checkInfixRecordTree)]
				
main = mapM_ (\(s,r) -> printf "%-30s: " s >> r) allTests			