{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures,
	OverlappingInstances, UndecidableInstances #-}

{-|
GenericPretty is a haskell library that provides support for automatic
derivation of pretty printing functions on user defined data types.
The Outputable library is used underneath, the work is done over SDoc types. 

The output provided by the library functions is identical to that of Prelude.show, 
except it has extra whitespace.

For examples of usage please see the README file. -}

module Text.PrettyPrint.GenericPretty(pp, prettyP, prettyStr, fullPP, outputTxt, outputStr, Generic, Out(..)) where

import Data.List
import Outputable
import GHC.Generics
import Pretty (fullRender, Mode(..), TextDetails(..), Doc)
import FastString
import Data.Char

-- | The class 'Out' is just a wrapper class for Outputable, which passes an extra parameter used to determine
-- when to wrap types up in parentheses
class Out a where
  -- | 'out' is the equivalent of Prelude.showsPrec
  -- it generates output identical to show, except for the extra whitespace
  out :: Int -> a -> SDoc
  -- default out method, converts the type into a sum of products and passes it on to the generic
  -- pretty printing functions, finally it concatenates all of the SDoc's
  default out :: (Generic a ,GOut (Rep a)) => Int -> a -> SDoc
  out n x = sep $ out1 (from x) Pref n False
  
  -- | 'outList' mimics the behaviour of Prelude.showList
  -- used mainly to output strings correctly, and not as lists of characters
  outList :: Int -> [a] -> SDoc
  outList n xs = brackets (fsep (punctuate comma (map (out n) xs)))

-- user-defined types that directly implement Outputable are handled here
-- n marks wether the type needs to be surrounded by parens or not
instance (Outputable a) => Out a where
	out n xs
		| n > 0 = parens $ ppr xs
		| otherwise = ppr xs

instance Out a => Outputable a where
	ppr = out 0
		
-- 'middle' return a list without it's first and last elements
-- except if the list has a single element, in which case it returns the list unchanged
middle :: [a] -> [a]
middle [] = []
middle [x] = [x]
middle (x:xs) = init xs

-- 'wrapParens' wraps the passed value in parens if the bool is true
-- we don't want a single paren to possibly take a whole line, so we concatenate them to the first
-- and last elements in the list, instead of just adding them to the list
wrapParens :: Bool -> [SDoc] -> [SDoc]
wrapParens _ [] = []
wrapParens False s = s
wrapParens True s
      | length s == 1 = [lparen <> head s <> rparen]
      |otherwise = [lparen <> head s] ++ middle s ++ [last s <> rparen]
		
-- The types of data we need to consider for product operator. Record, Prefix and Infix.
-- Tuples aren't considered since they're already instances of 'Out' and thus won't pass through that code.
data Type = Rec | Pref | Inf String

--'GOut' is a helper class used to output the Sum-of-Products type, since it has kind *->*, 
-- so can't be an instance of 'Out'
class GOut f where
  -- |'out1' is the (*->*) kind equivalent of 'out'
  out1 :: f x 		-- The sum of products representation of the user's custom type
		  -> Type   -- The type of multiplication. Record, Prefix or Infix.
		  -> Int    -- The operator precedence, determines wether to wrap stuff in parens.
		  -> Bool   -- A flag, marks wether the constructor directly above was wrapped in parens.
					-- Used to determine correct indentation
		  -> [SDoc] -- The result. Each SDoc could be on a newline, depending on available space.
  -- |'isNullary' marks nullary constructors, so that we don't put parens around them
  isNullary :: f x -> Bool
  
-- if empty, output nothing, this is a null constructor
instance GOut U1 where
  out1 _ _ _ _ = [empty]
  isNullary _ = True
  
-- ignore datatype meta-information
instance (GOut f, Datatype c) => GOut (M1 D c f) where
  out1 (M1 a) = out1 a
  isNullary (M1 a) = isNullary a
  
-- if there is a selector, display it and it's value + appropriate white space
instance (GOut f, Selector c) => GOut (M1 S c f) where
  out1 s@(M1 a) t d p
	| selector == "" = out1 a t d p
	| otherwise = (text selector <+> char '='):map (nest $ length selector + 3) (out1 a t 0 p)
	where
		selector = selName s
	
  isNullary (M1 a) = isNullary a

-- constructor
-- here the real type and parens flag is set and propagated forward via t and n, the precedence factor is updated
instance (GOut f, Constructor c) => GOut (M1 C c f) where
  out1 c@(M1 a) _ d p = 
    case fixity of
      -- if prefix add the constructor name, nest the result and possibly put it in parens
      Prefix -> wrapParens boolParens $ text name: makeMargins t boolParens (out1 a t 11 boolParens)
	  -- if infix possibly put in parens
      Infix _ m -> wrapParens (d>m) $ out1 a t (m+1) (d>m)
      where 
        boolParens = d>10 && (not $ isNullary a)
        name = checkInfix $ conName c
        fixity = conFixity c
        -- get the type of the data, Record, Infix or Prefix. 
        t = if conIsRecord c then Rec else
              case fixity of
                Prefix    -> Pref
                Infix _ _ -> Inf (conName c)
        
        --add whitespace and possible braces for records
        makeMargins :: Type -> Bool -> [SDoc] -> [SDoc]
        makeMargins _ _ [] = []
        makeMargins Rec b s 
            | length s == 1 = [nest (length name + 1) (lbrace <> head s <> rbrace)]
            | otherwise = nest (length name + 1) (lbrace <> head s) : 
							map (nest $ length name + 2) (middle s ++ [last s <> rbrace])
        makeMargins _ b s = map (nest $ length name + if b then 2 else 1) s
                
        -- check for infix operators that are acting like prefix ones due to records, put them in parens
        checkInfix :: String -> String
        checkInfix [] = []
        checkInfix (x:xs)
          | fixity == Prefix && (isAlphaNum x || x == '_') = (x:xs)
          | otherwise = "(" ++ (x:xs) ++ ")"
          
  isNullary (M1 a) = isNullary a
                 
-- ignore tagging, call out since these are concrete types
instance (Out f) => GOut (K1 t f) where
  out1 (K1 a) _ d _ = [out d a]
  isNullary _ = False

-- just continue to the corresponding side of the OR
instance (GOut f, GOut g) => GOut (f :+: g) where
  out1 (L1 a) t d p = out1 a t d p
  out1 (R1 a) t d p = out1 a t d p
  isNullary (L1 a) = isNullary a
  isNullary (R1 a) = isNullary a
  
-- output both sides of the product, possible separated by a comma or an infix operator
instance (GOut f, GOut g) => GOut (f :*: g) where
  out1 (f :*: g) t@Rec d p = init pfn ++ [last pfn <> comma] ++ pgn
    where 
      pfn = out1 f t d p
      pgn = out1 g t d p
	  
  -- if infix, nest the second value since it isn't nested in the constructor    
  out1 (f :*: g) t@(Inf s) d p = init pfn ++ [last pfn <+> text s] ++ checkIndent pgn
    where
      pfn = out1 f t d p
      pgn = out1 g t d p
      
      -- if the second value of the :*: is in parens, nest it
	  -- needs to get the string representation of the first elements in the left and right SDoc lists 
	  -- to be able to determine the correct indentation
      checkIndent :: [SDoc] -> [SDoc]
      checkIndent [] = []
      checkIndent m@(x:xs)
          | parens == 0 = m
          | otherwise = map (nest $ cons + 1 + parenSpace) m
            where
              parenSpace = if p then 1 else 0
              strG = showSDocOneLine x
              strF = showSDocOneLine (head pfn)
              parens = length $ takeWhile (== '(') strG
              cons = length $ takeWhile( /= ' ') (dropWhile(== '(') strF)              
              
  out1 (f :*: g) t@Pref n p = out1 f t n p ++ out1 g t n p
  
  isNullary _ = False
				
-- | 'fullPP' is a fully customizable Pretty Printer.

fullPP :: (Out a) => a 							-- ^The value to pretty print
					 -> PprStyle 				-- ^The Outputable library style to use /(default is defaultUserStyle)/
					 -> Mode 					-- ^The 'Pretty' library style(mode) to use /(default is PageMode)/
					 -> Int 					-- ^The maximum line length
					 -> Float 					-- ^The number of ribbons per line
					 -> (TextDetails -> b -> b) -- ^Function that handles the text conversion /(default is 'outputTxt')/
					 -> b 						-- ^The end element of the result /( eg: "" or putChar('\n') )/
					 -> b						-- ^The pretty printed result
fullPP a pstyle mode len rib td end = fullRender mode len rib td end doc
  where
    doc = withPprStyleDoc pstyle (out 0 a)

-- | 'outputTxt' transforms the text into strings and outputs it directly.
-- This is one example of a function that can handle the text conversion for 'fullPP'.
outputTxt :: TextDetails -> IO() -> IO()
outputTxt td act =  do
                      putStr $ decode td
                      act
  where
    decode :: TextDetails -> String
    decode (PStr s1) = unpackFS s1
    decode (LStr s1 _) = unpackLitString s1
    decode (Chr c)  = [c]
    decode (Str s) = s
    
-- | 'outputStr' just leaves the text as a string.
-- Another example of a function that can handle the text conversion for 'fullPP'.
outputStr :: TextDetails -> String -> String
outputStr td str = decode td ++ str
  where
    decode :: TextDetails -> String
    decode (PStr s1) = unpackFS s1
    decode (LStr s1 _) = unpackLitString s1
    decode (Chr c)  = [c]
    decode (Str s) = s
    
-- | 'prettyStr' returns the result as a string. 
-- The returned value is identical to one made by Prelude.show, except for the extra whitespace
prettyStr :: (Out a) => a -> String
prettyStr a = fullPP a defaultUserStyle PageMode 80 1.5 outputStr ""

-- | 'prettyP' is a partly customizable Pretty Printer
-- It takes the line length and ribbons per line as parameters
prettyP :: (Out a) => Int -> Float -> a -> IO()
prettyP len rib a = fullPP a defaultUserStyle PageMode len rib outputTxt (putChar '\n')

-- | 'pp' is the default Pretty Printer,
-- it uses a line length of 80 and 1.5 ribbons per line /(= 53 non-whitespace chars per line)/
-- where ribbon is defined as the maximum length of text, excluding whitespace, on a single line
pp :: (Out a) => a -> IO()
pp = prettyP 80 1.5

-- define some instances of Out making sure to generate output identical to 'show' modulo the extra whitespace
instance Out Char where
	out _ a = char '\'' <> (text.middle.show $ a) <> char '\''
	outList _ xs = text $ show xs
			
instance Out Integer where
	out n x
		| n/=0 && x<0 = parens $ integer x
		| otherwise = integer x
  
instance Out a => Out [a] where
  out = outList
  
instance Out Bool where
    out _ True = ptext (sLit "True")
    out _ False = ptext (sLit "False")

instance Out Int where
   out n x
	| n/=0 && x<0 = parens $ int x
	| otherwise = int x

instance Out a => Out (Maybe a) where
  out n Nothing = ptext (sLit "Nothing")
  out n (Just x)
	| n/=0 = parens result
	|otherwise = result
	  where
		result = ptext (sLit "Just") <+> out 10 x

instance (Out a, Out b) => Out (Either a b) where
  out n (Left x)
	| n/=0 = parens result
	| otherwise = result
	  where
		result = ptext (sLit "Left")  <+> out 10 x
  out n (Right y)
	| n/=0 = parens result
	| otherwise = result
	  where
		result = ptext (sLit "Right") <+> out 10 y

instance (Out a, Out b) => Out (a, b) where
    out _ (a,b) = parens (sep [out 0 a <> comma, out 0 b])
	
instance (Out a, Out b, Out c) => Out (a, b, c) where
    out _ (a,b,c) = parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c])

instance (Out a, Out b, Out c, Out d) => Out (a, b, c, d) where
    out _ (a,b,c,d) = parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d])

instance (Out a, Out b, Out c, Out d, Out e) =>	 Out (a, b, c, d, e) where
    out _ (a,b,c,d,e) = parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, out 0 e])

instance (Out a, Out b, Out c, Out d, Out e, Out f) 
	=> Out (a, b, c, d, e, f) where
		 out _ (a, b, c, d, e, f) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, 
						 out 0 d <> comma, out 0 e <> comma, out 0 f])
      
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g) 
	=> Out (a, b, c, d, e, f, g) where
		 out _ (a, b, c, d, e, f, g) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, 
                   out 0 d <> comma, out 0 e <> comma, out 0 f <> comma, out 0 g])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h) 
	=> Out (a, b, c, d, e, f, g, h) where
		 out _ (a, b, c, d, e, f, g, h) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, 
                   out 0 d <> comma, out 0 e <> comma, out 0 f <> comma, out 0 g <> comma, out 0 h])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h, Out i) 
	=> Out (a, b, c, d, e, f, g, h, i) where
		 out _ (a, b, c, d, e, f, g, h, i) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, 
                   out 0 e <> comma, out 0 f <> comma, out 0 g <> comma, out 0 h <> comma, out 0 i])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h, Out i, Out j) 
	=> Out (a, b, c, d, e, f, g, h, i, j) where
		 out _ (a, b, c, d, e, f, g, h, i, j) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, 
                   out 0 e <> comma, out 0 f <> comma, out 0 g <> comma, out 0 h <> comma, out 0 i <> comma, out 0 j])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h, Out i, Out j, Out k) 
	=> Out (a, b, c, d, e, f, g, h, i, j, k) where
		 out _ (a, b, c, d, e, f, g, h, i, j, k) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, out 0 e<> comma, 
                   out 0 f <> comma, out 0 g <> comma, out 0 h <> comma, out 0 i <> comma, out 0 j <> comma, out 0 k])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h, Out i, Out j, Out k, Out l) 
	=> Out (a, b, c, d, e, f, g, h, i, j, k, l) where
		 out _ (a, b, c, d, e, f, g, h, i, j, k, l) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, out 0 e <> comma, 
					out 0 f <> comma, out 0 g <> comma, out 0 h <> comma, out 0 i <> comma, out 0 j <> comma, 
					out 0 k <> comma, out 0 l])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h, Out i, Out j, Out k, Out l, Out m) 
	=> Out (a, b, c, d, e, f, g, h, i, j, k, l, m) where
		 out _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, out 0 e <> comma, 
                   out 0 f <> comma, out 0 g <> comma, out 0 h <> comma, out 0 i <> comma, out 0 j <> comma, 
                   out 0 k <> comma, out 0 l <> comma, out 0 m])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h, Out i, Out j, Out k, Out l, Out m, Out n) 
	=> Out (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
		 out _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, out 0 e <> comma, 
                   out 0 f <> comma, out 0 g <> comma, out 0 h <> comma, out 0 i <> comma, out 0 j <> comma, 
                   out 0 k <> comma, out 0 l <> comma, out 0 m <> comma, out 0 n])
              
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h, Out i, Out j, Out k, Out l, Out m, Out n, Out o) 
	=> Out (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
		 out _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = 
			parens (sep [out 0 a <> comma, out 0 b <> comma, out 0 c <> comma, out 0 d <> comma, out 0 e <> comma, 
                   out 0 f <> comma, out 0 g <> comma, out 0 h <> comma, out 0 i <> comma, out 0 j <> comma, 
                   out 0 k <> comma, out 0 l <> comma, out 0 m <> comma, out 0 n <> comma, out 0 o])