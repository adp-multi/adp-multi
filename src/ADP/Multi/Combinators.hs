{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module ADP.Multi.Combinators where
import Data.Array
import Data.Char (ord)

-- # Lexical parsers


type Subword2  = (Int,Int,Int,Int)
type Parser2 a b = Array Int a -> Subword2 -> [b]

data EPS = EPS

data Ranges = RangeMap Subword2 [Ranges] deriving Show

empty2 :: Parser2 a ()
empty2 _ (i,j,k,l) = [() | i == j && k == l]

anychars :: Parser2 a (a,a)
anychars z (i,j,k,l) = [(z!j, z!l) | i+1 == j && k+1 == l]

chars :: Eq a => a -> a -> Parser2 a (a,a)
chars c1 c2 = anychars `with2` charsFilter where
        charsFilter z (_,j,_,l) = z!j == c1 && z!l == c2
        
charLeftOnly :: Eq a => a -> Parser2 a (a,EPS)
charLeftOnly c z (i,j,k,l) = [(c, EPS) | i+1 == j && k == l && z!j == c]

charRightOnly :: Eq a => a -> Parser2 a (EPS,a)
charRightOnly c z (i,j,k,l) = [(EPS, c) | i == j && k+1 == l && z!l == c]


class Parseable p a b | p -> a b where
    toParser :: p -> Parser2 a b
    
instance Parseable (Parser2 a b) a b where
    toParser p = p

-- TODO the use of just 'a' needs UndecidableInstances, is this necessary?
instance Parseable () a () where
    toParser _ = empty2 

instance Eq a => Parseable (a,a) a (a,a) where
    toParser (c1,c2) = chars c1 c2
    
instance Eq a => Parseable (EPS,a) a (EPS,a) where
    toParser (_,c) = charRightOnly c
    
instance Eq a => Parseable (a,EPS) a (a,EPS) where
    toParser (c,_) = charLeftOnly c

-- # Parser combinators

infixr 5 ||| 
(|||) :: Parser2 a b -> Parser2 a b -> Parser2 a b
(|||) r q z (i,j,k,l) = r z (i,j,k,l) ++ q z (i,j,k,l)

infix 8 <<<
(<<<) :: Parseable p a b => (b -> c) -> p -> [Ranges] -> Parser2 a c
(<<<) f p [] z (i,j,k,l) = map f (q z (i,j,k,l)) where
                                  q = toParser p
(<<<) _ _ r _ _ = error $ "something went wrong... the ranges list should be empty: " ++ show r

infix 6 >>>
(>>>) :: Rewriting c => ([Ranges] -> Parser2 a b) -> c -> Parser2 a b
(>>>) p f z subword = [ result | RangeMap sub rest <- constructRanges f subword, result <- p rest z sub ]  

infixl 7 ~~~
(~~~) :: Parseable p a b => ([Ranges] -> Parser2 a (b -> c)) -> p -> [Ranges] -> Parser2 a c
(~~~) p q ranges z subword = [ pr qr | qr <- q' z subword, RangeMap sub rest <- ranges, pr <- p rest z sub ] where
                             q' = toParser q

class Rewriting f where
  constructRanges :: f -> Subword2 -> [Ranges]

instance (Num a, Eq a) => Rewriting ((a,a) -> ([a],[a])) where
  constructRanges f (i,j,k,l) = case f (1,2) of
        ([1],[2]) -> [RangeMap (i,j,k,l) []]
        ([2],[1]) -> [RangeMap (k,l,i,j) []]
        ([1,2],[]) -> undefined
        ([2,1],[]) -> undefined
        ([],[1,2]) -> undefined
        ([],[2,1]) -> undefined
  
instance Rewriting ((a,a) -> (a,a) -> ([a],[a])) where
  constructRanges f subword = undefined


type Filter2 a = Array Int a -> Subword2 -> Bool
with2 :: Parser2 a b -> Filter2 a -> Parser2 a b
with2 q c z subword = if c z subword then q z subword else []

axiom'        :: Int -> Array Int a -> Parser2 a b -> [b]
axiom' l z ax   =  ax z (0,l,0,l) -- TODO does that still make sense?

-- # Tabulation

-- four-dimensional tabulation
table2     :: Int -> Parser2 a b -> Parser2 a b
table2 n q z = (!) $ array ((0,0,0,0),(n,n,n,n))
                   [((i,j,k,l),q z (i,j,k,l)) | i <- [0..n], j <- [i..n], k <- [0..n], l <- [k..n]]

-- # TESTS

type TestAlgebra alphabet answer = (
        () -> answer,
        (EPS, alphabet) -> answer,
        (alphabet, alphabet) -> answer,
        (alphabet, alphabet) -> (alphabet, alphabet) -> answer,
        answer -> answer -> answer
   )

testAlg :: TestAlgebra Char Int
testAlg = (nil, left, f, f2, f3) where
   nil () = 0
   left (EPS,c) = ord c
   f (l,r) = ord l + ord r
   f2 (l,r) (l2,r2) = ord l + ord r + ord l2 + ord r2
   f3 l r = l + r
   
testGram :: TestAlgebra Char answer -> String -> [answer]
testGram alg inp = axiom k where
  (nil, left, f, f2, f3) = alg
  
  f3' :: (a,a) -> (a,a) -> ([a],[a]) 
  f3' (p1,p2) (k1,k2) = ([k1,p1],[p2,k2])

  k = f3 <<< p ~~~ k >>> f3'
  
  f' :: (a,a) -> ([a],[a]) 
  f' (c1,c2) = ([c1],[c2]) -- "identical" function
  p = tabulated (
      f <<< ('a', 'u') >>> f' |||
      f <<< ('u', 'a') >>> f' |||
      f <<< ('c', 'g') >>> f' |||
      f <<< ('g', 'c') >>> f' |||
      f <<< ('g', 'u') >>> f' |||
      f <<< ('u', 'g') >>> f' 
      )
      
  test = f   <<< ('a', 'u') >>> f' |||
         nil <<< ()         >>> f' |||
         left <<< (EPS, 'u') >>> f'
      
  -- or using our basepair parser constructed with a filter:
  
  p2 = f <<< basepair >>> f'

  z         = mk inp
  (_,n)     = bounds (z)
  
  tabulated = table2 n
  basepair  = anychars `with2` basepairing
  
  basepairing :: Filter2 Char
  basepairing z (i,j,k,l) = i+1 == j && k+1 == l && isBasepair (z!(i+1), z!(k+1))
  
  axiom     = axiom' n z
  
isBasepair ('a','u') = True
isBasepair ('u','a') = True
isBasepair ('c','g') = True
isBasepair ('g','c') = True
isBasepair ('g','u') = True
isBasepair ('u','g') = True
isBasepair _         = False
  
test = testGram testAlg "lala"


-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,n) (zip [1..n] xs) where n = length xs