{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ADP.Multi.Combinators where
import Data.Array
import Data.Char (ord)

-- # Lexical parsers


type Subword2  = (Int,Int,Int,Int)
type Parser2 b = Subword2 -> [b]

data Ranges = RangeMap Subword2 [Ranges] deriving Show

empty2 :: Parser2 ()
empty2 (i,j,k,l) = [() | i == j && k == l]

anychars :: Array Int a -> Parser2 (a,a)
anychars z (i,j,k,l) = [(z!j, z!l) | i+1 == j && k+1 == l]

--chars' (z1,z2) c1 c2 (i,j,k,l) = [(z1!j, z2!l) | i+1 == j && k+1 == l && z1!j == c1 && z2!l == c2]
chars' :: Eq a => Array Int a -> a -> a -> Parser2 (a,a)
chars' z c1 c2  = anychars z `with2` charsFilter where
        charsFilter (_,j,_,l) = z!j == c1 && z!l == c2


{-      
class Parseable p a | p -> a where
    toParser :: p -> Parser2 a
    
instance Parseable (Parser2 b) b where
    toParser p = p

instance Parseable () () where
    toParser _ = empty2 

instance Parseable (a,a) (a,a) where
    toParser (c1,c2) = 
    -- TODO we need to have access to the input here
    --      the Parser2 type needs to be extended to include Array Int a
    --      at the end, the input is given to axiom which hands it down to the parsers

tt = toParser (anychars (mk "l"))
tt2 = toParser empty2
tt3 = toParser ()
tt4 = toParser ('a','c')
-}      
-- TODO parser where just one element is empty

-- # Parser combinators

infixr 5 ||| 
(|||) :: Parser2 b -> Parser2 b -> Parser2 b
(|||) r q (i,j,k,l) = r (i,j,k,l) ++ q (i,j,k,l)

infix 8 <<<
(<<<) :: (b -> c) -> Parser2 b -> [Ranges] -> Parser2 c
(<<<) f q [] (i,j,k,l) = map f (q (i,j,k,l))
(<<<) _ _ r _ = error $ "something went wrong... the ranges list should be empty: " ++ show r

infix 6 >>>
(>>>) :: Rewriting b => ([Ranges] -> Parser2 a) -> b -> Parser2 a
(>>>) p f subword = [ result | RangeMap sub rest <- constructRanges f subword, result <- p rest sub ]  

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


infixl 7 ~~~
(~~~) :: ([Ranges] -> Parser2 (b -> c)) -> Parser2 b -> [Ranges] -> Parser2 c
(~~~) p q ranges subword = [ pr qr | qr <- q subword, RangeMap sub rest <- ranges, pr <- p rest sub ]


type Filter2 = Subword2 -> Bool
with2 :: Parser2 b -> Filter2 -> Parser2 b
with2 q c (i,j,k,l) = if c (i,j,k,l) then q (i,j,k,l)  else []

axiom'        :: Int -> Parser2 b -> [b]
axiom' l ax   =  ax (0,l,0,l) -- TODO does that still make sense?

-- # Tabulation

-- four-dimensional tabulation
table2     :: Int -> Parser2 b -> Parser2 b
table2 n q =  (!) $ array ((0,0,0,0),(n,n,n,n))
                   [((i,j,k,l),q (i,j,k,l)) | i <- [0..n], j <- [i..n], k <- [0..n], l <- [k..n]]

-- # TESTS

type TestAlgebra alphabet answer = (
        () -> answer,
        (alphabet, alphabet) -> answer,
        (alphabet, alphabet) -> (alphabet, alphabet) -> answer,
        answer -> answer -> answer
   )

testAlg :: TestAlgebra Char Int
testAlg = (nil, f, f2, f3) where
   nil _ = 0
   f (l,r) = ord l + ord r
   f2 (l,r) (l2,r2) = ord l + ord r + ord l2 + ord r2
   f3 l r = l + r
   
testGram :: TestAlgebra Char answer -> String -> [answer]
testGram alg inp = axiom k where
  (nil, f, f2, f3) = alg
  
  f3' :: (a,a) -> (a,a) -> ([a],[a]) 
  f3' (p1,p2) (k1,k2) = ([k1,p1],[p2,k2])

  k = f3 <<< p ~~~ k >>> f3'
  
  f' :: (a,a) -> ([a],[a]) 
  f' (c1,c2) = ([c1],[c2]) -- "identical" function
  p = tabulated (
      f <<< chars 'a' 'u' >>> f' |||
      f <<< chars 'u' 'a' >>> f' |||
      f <<< chars 'c' 'g' >>> f' |||
      f <<< chars 'g' 'c' >>> f' |||
      f <<< chars 'g' 'u' >>> f' |||
      f <<< chars 'u' 'g' >>> f'
      )
      
  -- or using our basepair parser constructed with a filter:
  
  p2 = f <<< basepair >>> f'

  z         = mk inp
  (_,n)     = bounds (z)
  
  chars = chars' z
  tabulated = table2 n
  basepair  = anychars z `with2` basepairing
  
  basepairing :: Filter2
  basepairing (i,j,k,l) = i+1 == j && k+1 == l && isBasepair (z!(i+1), z!(k+1))
  
  axiom     = axiom' n
  
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