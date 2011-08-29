{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures #-}

{-|
  GenericPretty is a Haskell library that supports automatic
  derivation of pretty printing functions on user defined data
  types.
	
	The output provided is a pretty printed version of that provided by
  'Prelude.show'.  That is, rendering the document provided by this pretty
  printer yields an output identical to that of 'Prelude.show', except
  for extra whitespace.
		
	For examples of usage please see the README file included in the package.
  
  For more information see the HackageDB project page: <http://hackage.haskell.org/package/GenericPretty-1.1.9> 
-}

module Text.PrettyPrint.GenericPretty 
                      (
                      Out(..), 
                      pp, ppLen, ppStyle, pretty, prettyLen, prettyStyle, fullPP, 
                      Generic,
                      outputIO, outputStr
                      ) where

import Data.List
import GHC.Generics
import Data.Char
import FastString
import Text.PrettyPrint.MyPretty

-- | The class 'Out' is the equivalent of 'Prelude.Show'
--
-- It provides conversion of values to pretty printable Pretty.Doc's.
--
-- Minimal complete definition: 'docPrec' or 'doc'.
--
-- Derived instances of 'Out' have the following properties
--
-- * The result of 'docPrec' is a syntactically correct Haskell
--   expression containing only constants, given the fixity
--   declarations in force at the point where the type is declared.
--   It contains only the constructor names defined in the data type,
--   parentheses, and spaces.  When labelled constructor fields are
--   used, braces, commas, field names, and equal signs are also used.
--
-- * If the constructor is defined to be an infix operator, then
--   'docPrec' will produce infix applications of the constructor.
--
-- * the representation will be enclosed in parentheses if the
--   precedence of the top-level constructor in @x@ is less than @d@
--   (associativity is ignored).  Thus, if @d@ is @0@ then the result
--   is never surrounded in parentheses; if @d@ is @11@ it is always
--   surrounded in parentheses, unless it is an atomic expression.
--
-- * If the constructor is defined using record syntax, then 'docPrec'
--   will produce the record-syntax form, with the fields given in the
--   same order as the original declaration.
--
-- For example, given the declarations
--
-- 
-- > data Tree a =  Leaf a  |  Node (Tree a) (Tree a) deriving (Generic)
--
-- The derived instance of 'Out' is equivalent to:
--
-- > instance (Out a) => Out (Tree a) where
-- >  
-- >         docPrec d (Leaf m) = Pretty.sep $ wrapParens (d > appPrec) $
-- >              text "Leaf" : [nest (constrLen + parenLen) (docPrec (appPrec+1) m)]
-- >           where appPrec = 10
-- >                 constrLen = 5;
-- >                 parenLen = if(d > appPrec) then 1 else 0
-- > 
-- >         docPrec d (Node u v) = Pretty.sep $ wrapParens (d > appPrec) $
-- >              text "Node" : 
-- >              nest (constrLen + parenLen) (docPrec (appPrec+1) u) : 
-- >              [nest (constrLen + parenLen) (docPrec (appPrec+1) v)]
-- >           where appPrec = 10
-- >                 constrLen = 5
-- >                 parenLen = if(d > appPrec) then 1 else 0

class Out a where
  -- | 'docPrec' is the equivalent of 'Prelude.showsPrec'.
  --
  -- Convert a value to a pretty printable 'Pretty.Doc'.
  docPrec :: Int  -- ^ the operator precedence of the enclosing
                  -- context (a number from @0@ to @11@).
                  -- Function application has precedence @10@. 
          -> a    -- ^ the value to be converted to a 'String'
          -> Doc  -- ^ the resulting Doc
  default docPrec :: (Generic a ,GOut (Rep a)) => Int -> a -> Doc
  docPrec n x = sep $ out1 (from x) Pref n False
  
  -- | 'doc' is the equivalent of 'Prelude.show'
  --
  -- This is a specialised variant of 'docPrec', using precedence context zero.
  doc :: a -> Doc
  default doc :: (Generic a ,GOut (Rep a)) => a -> Doc
  doc x = sep $ out1 (from x) Pref 0 False
  
  -- | 'docList' is the equivalent of 'Prelude.showList'.
  --
  -- The method 'docList' is provided to allow the programmer to
  -- give a specialised way of showing lists of values.
  -- For example, this is used by the predefined 'Out' instance of
  -- the 'Char' type, where values of type 'String' should be shown
  -- in double quotes, rather than between square brackets.
  docList :: [a] -> Doc
  docList = docListWith doc

-- used to define docList, creates output identical to that of show for general list types
docListWith :: (a -> Doc) -> [a] -> Doc
docListWith f = brackets . fcat . punctuate comma . map f
  
-- returns a list without it's first and last elements
-- except if the list has a single element, in which case it returns the list unchanged
middle :: [a] -> [a]
middle [] = []
middle [x] = [x]
middle (x:xs) = init xs

-- |Utility function used to wrap the passed value in parens if the bool is true.
wrapParens :: Bool -> [Doc] -> [Doc]
wrapParens _ [] = []
wrapParens False s = s
wrapParens True s
      | length s == 1 = [lparen <> head s <> rparen]
      |otherwise = [lparen <> head s] ++ middle s ++ [last s <> rparen]
      
-- show the whole document in one line
showDocOneLine :: Doc -> String
showDocOneLine = fullRender OneLineMode 1 1 outputStr ""
		
-- The types of data we need to consider for product operator. Record, Prefix and Infix.
-- Tuples aren't considered since they're already instances of 'Out' and thus won't pass through that code.
data Type = Rec | Pref | Inf String

--'GOut' is a helper class used to output the Sum-of-Products type, since it has kind *->*, 
-- so can't be an instance of 'Out'
class GOut f where
  -- |'out1' is the (*->*) kind equivalent of 'docPrec'
  out1 :: f x 	-- The sum of products representation of the user's custom type
		  -> Type   -- The type of multiplication. Record, Prefix or Infix.
		  -> Int    -- The operator precedence, determines wether to wrap stuff in parens.
		  -> Bool   -- A flag, marks wether the constructor directly above was wrapped in parens.
                -- Used to determine correct indentation
		  -> [Doc]  -- The result. Each Doc could be on a newline, depending on available space.
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
        makeMargins :: Type -> Bool -> [Doc] -> [Doc]
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
                 
-- ignore tagging, call docPrec since these are concrete types
instance (Out f) => GOut (K1 t f) where
  out1 (K1 a) _ d _ = [docPrec d a]
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
      
      -- if the second value of the :*: is in parens, nest it, otherwise just check for an extra paren space
      -- needs to get the string representation of the first elements in the left and right Doc lists 
      -- to be able to determine the correct indentation
      checkIndent :: [Doc] -> [Doc]
      checkIndent [] = []
      checkIndent m@(x:xs)
          | parens == 0 = if p then map (nest 1) m else m
          | otherwise = map (nest $ cons + 1 + parenSpace) m
            where
              parenSpace = if p then 1 else 0
              strG = showDocOneLine x
              strF = showDocOneLine (head pfn)
              parens = length $ takeWhile (== '(') strG
              cons = length $ takeWhile( /= ' ') (dropWhile(== '(') strF)              
              
  out1 (f :*: g) t@Pref n p = out1 f t n p ++ out1 g t n p
  
  isNullary _ = False
				
-- | 'fullPP' is a fully customizable Pretty Printer
--
-- Every other pretty printer just gives some default values to 'fullPP' 
fullPP :: (Out a) => (TextDetails -> b -> b)  -- ^Function that handles the text conversion /(eg: 'outputIO')/
       -> b      -- ^The end element of the result /( eg: "" or putChar('\n') )/
       -> Style  -- ^The pretty printing 'Text.PrettyPrint.MyPretty.Style' to use
       -> a      -- ^The value to pretty print
       -> b      -- ^The pretty printed result
fullPP td end s a = fullRender (mode s) (lineLength s) (ribbonsPerLine s) td end doc
  where
    doc = docPrec 0 a
    
defaultStyle :: Style
defaultStyle = Style {mode = PageMode, lineLength = 80, ribbonsPerLine = 1.5}

-- | Utility function that handles the text conversion for 'fullPP'.
--
-- 'outputIO' transforms the text into 'String's and outputs it directly.
outputIO :: TextDetails -> IO() -> IO()
outputIO td act =  do
                      putStr $ decode td
                      act
  where
    decode :: TextDetails -> String
    decode (PStr s1) = unpackFS s1
    decode (LStr s1 _) = unpackLitString s1
    decode (Chr c)  = [c]
    decode (Str s) = s
    
-- | Utility function that handles the text conversion for 'fullPP'. 
--
--'outputStr' just leaves the text as a 'String' which is usefull if you want 
-- to further process the pretty printed result.
outputStr :: TextDetails -> String -> String
outputStr td str = decode td ++ str
  where
    decode :: TextDetails -> String
    decode (PStr s1) = unpackFS s1
    decode (LStr s1 _) = unpackLitString s1
    decode (Chr c)  = [c]
    decode (Str s) = s

-- | Customizable pretty printer 
--
-- Takes a user defined 'Text.PrettyPrint.MyPretty.Style' as a parameter and uses 'outputStr' to obtain the result
-- Equivalent to:
--
-- > fullPP outputStr ""
prettyStyle :: (Out a) => Style -> a -> String
prettyStyle = fullPP outputStr ""

-- | Semi-customizable pretty printer. 
--
-- Equivalent to:
--
-- > prettyStyle customStyle
-- 
-- Where customStyle uses the specified line length, mode = PageMode and ribbonsPerLine = 1. 
prettyLen :: (Out a) => Int -> a -> String
prettyLen l = prettyStyle customStyle
  where
    customStyle = Style {mode = PageMode, lineLength = l, ribbonsPerLine = 1}

-- | The default pretty printer returning 'String's
--
--  Equivalent to
--
-- > prettyStyle defaultStyle
--
-- Where defaultStyle = (mode=PageMode, lineLength=80, ribbonsPerLine=1.5)
pretty :: (Out a) => a -> String
pretty = prettyStyle defaultStyle

-- | Customizable pretty printer.
-- 
-- Takes a user defined 'Text.PrettyPrint.MyPretty.Style' as a parameter and uses 'outputIO' to obtain the result
-- Equivalent to:
--
-- > fullPP outputIO (putChar '\n')
ppStyle :: (Out a) => Style -> a -> IO()
ppStyle = fullPP outputIO (putChar '\n')

-- | Semi-customizable pretty printer. 
--
-- Equivalent to:
--
-- > ppStyle customStyle
-- 
-- Where customStyle uses the specified line length, mode = PageMode and ribbonsPerLine = 1. 
ppLen :: (Out a) => Int -> a -> IO()
ppLen l = ppStyle customStyle
  where
    customStyle = Style {mode = PageMode, lineLength = l, ribbonsPerLine = 1}

-- | The default Pretty Printer,
--
--  Equivalent to:
--
-- > ppStyle defaultStyle
--
-- Where defaultStyle = (mode=PageMode, lineLength=80, ribbonsPerLine=1.5)
pp :: (Out a) => a -> IO()
pp = ppStyle defaultStyle

 
-- define some instances of Out making sure to generate output identical to 'show' modulo the extra whitespace
instance Out () where
  doc _ = text "()"
  docPrec _ = doc
  
instance Out Char where
  doc a = char '\'' <> (text.middle.show $ a) <> char '\''
  docPrec _ = doc
  docList xs = text $ show xs
  			
instance Out Int where
  docPrec n x
      | n/=0 && x<0 = parens $ int x
      | otherwise = int x
  doc = docPrec 0

instance Out Float where
  docPrec n x
      | n/=0 && x<0 = parens $ float x
      | otherwise = float x
  doc = docPrec 0

instance Out Double where
  docPrec n x
      | n/=0 && x<0 = parens $ double x
      | otherwise = double x
  doc = docPrec 0
  
instance Out a => Out [a] where
  doc = docList
  docPrec _ = doc
  
instance Out Bool where
    doc True = text "True"
    doc False = text "False"
    docPrec _ = doc

instance Out a => Out (Maybe a) where
  docPrec n Nothing = text "Nothing"
  docPrec n (Just x)
	| n/=0 = parens result
	|otherwise = result
	  where
		result = text "Just" <+> docPrec 10 x
  doc = docPrec 0

instance (Out a, Out b) => Out (Either a b) where
  docPrec n (Left x)
	| n/=0 = parens result
	| otherwise = result
	  where
		result = text "Left"  <+> docPrec 10 x
  docPrec n (Right y)
	| n/=0 = parens result
	| otherwise = result
	  where
		result = text "Right" <+> docPrec 10 y
  doc = docPrec 0

instance (Out a, Out b) => Out (a, b) where
    doc (a,b) = parens (sep [doc a <> comma, doc b])
    docPrec _ = doc
	
instance (Out a, Out b, Out c) => Out (a, b, c) where
    doc (a,b,c) = parens (sep [doc a <> comma, doc b <> comma, doc c])
    docPrec _ = doc

instance (Out a, Out b, Out c, Out d) => Out (a, b, c, d) where
    doc (a,b,c,d) = parens (sep [doc a <> comma, doc b <> comma, doc c <> comma, doc d])
    docPrec _ = doc

instance (Out a, Out b, Out c, Out d, Out e) =>	 Out (a, b, c, d, e) where
    doc (a,b,c,d,e) = parens (sep [doc a <> comma, doc b <> comma, doc c <> comma, doc d <> comma, doc e])
    docPrec _ = doc

instance (Out a, Out b, Out c, Out d, Out e, Out f) 
	=> Out (a, b, c, d, e, f) where
      doc (a, b, c, d, e, f) = 
        parens (sep [ doc a <> comma, doc b <> comma, doc c <> comma, 
                      doc d <> comma, doc e <> comma, doc f])
      docPrec _ = doc
      
instance (Out a, Out b, Out c, Out d, Out e, Out f, Out g) 
	=> Out (a, b, c, d, e, f, g) where
      doc (a, b, c, d, e, f, g) = 
        parens (sep [ doc a <> comma, doc b <> comma, doc c <> comma, 
                      doc d <> comma, doc e <> comma, doc f <> comma, doc g])
      docPrec _ = doc