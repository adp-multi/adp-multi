{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module ADP.Multi.Combinators where
import Debug.Trace
import Control.Exception
import Data.Array
import Data.Char (ord)
import Data.List (find, elemIndex)

-- # Lexical parsers

type Subword  = (Int,Int)
type Subword2  = (Int,Int,Int,Int)
type Parser2 a b = Array Int a -> Subword2 -> [b]

data EPS = EPS

data Ranges = RangeMap Subword2 [Ranges] deriving Show

empty2 :: Parser2 a ()
empty2 _ (i,j,k,l) = [() | i == j && k == l]

anychars :: Parser2 a (a,a)
anychars z (i,j,k,l) = [(z!j, z!l) | i+1 == j && k+1 == l]

chars :: Eq a => a -> a -> Parser2 a (a,a)
chars c1 c2 z (i,j,k,l) = [(z!j, z!l) | i+1 == j && k+1 == l && z!j == c1 && z!l == c2] 
        
charLeftOnly :: Eq a => a -> Parser2 a (a,EPS)
charLeftOnly c z (i,j,k,l) = [(c, EPS) | i+1 == j && k == l && z!j == c]

charRightOnly :: Eq a => a -> Parser2 a (EPS,a)
charRightOnly c z (i,j,k,l) = [(EPS, c) | i == j && k+1 == l && z!l == c]


class Parseable p a b | p -> a, p -> b where
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

-- TODO the dimension of the resulting parser should result from c
infix 6 >>>
(>>>) :: Rewriting c => ([Ranges] -> Parser2 a b) -> c -> Parser2 a b
(>>>) _ _ _ a | trace (">>> " ++ show a) False = undefined
(>>>) p f z subword = [ result | RangeMap sub rest <- constructRanges f subword, result <- p rest z sub ]  

-- TODO parsers of different dim's should be mixable
-- this seems hard to do ATM, but isn't really a problem for now as lower dimensions can be
-- simulated by higher dimensions by leaving some tuple elements of the rewriting rules empty
infixl 7 ~~~
(~~~) :: Parseable p a b => ([Ranges] -> Parser2 a (b -> c)) -> p -> [Ranges] -> Parser2 a c
(~~~) p q ranges z subword = [ pr qr | qr <- q' z subword, RangeMap sub rest <- ranges, pr <- p rest z sub ] where
                             q' = toParser q

class Rewriting f where
  constructRanges :: f -> Subword2 -> [Ranges]

-- 2-dim to 2-dim with 1 tuple
instance (Num a, Eq a) => Rewriting ((a,a) -> ([a],[a])) where
  constructRanges f (i,j,k,l) = case f (1,2) of
        ([1],[2]) -> [RangeMap (i,j,k,l) []]
        ([2],[1]) -> [RangeMap (k,l,i,j) []]
        ([1,2],[]) -> [RangeMap (i,x,x,j) [] | k == l, x <- [i..j]]
        ([2,1],[]) -> [RangeMap (x,j,i,x) [] | k == l, x <- [i..j]]
        ([],[1,2]) -> [RangeMap (k,x,x,l) [] | i == j, x <- [k..l]]
        ([],[2,1]) -> [RangeMap (x,k,l,x) [] | k == l, x <- [k..l]]
        _ -> error "invalid rewriting function, each argument must appear exactly once"

-- 2-dim to 2-dim  with many tuples
instance (Num a, Eq a, Show a) => Rewriting ([(a,a)] -> ([(a,a)],[(a,a)])) where
  constructRanges _ b | trace ("constructRanges " ++ show b) False = undefined
  constructRanges f (i,j,k,l) = 
        assert (i <= j && j <= k && k <= l) $
        let (left,right) = f [(1,1),(1,2),(2,1),(2,2)] -- TODO this is a special case for 2 symbols
            remainingSymbols = [2,1]
            rangeDesc = [(i,j,left),(k,l,right)]
            rangeDescFiltered = filterEmptyRanges rangeDesc
        in if any (\(m,n,d) -> null d && m /= n) rangeDesc then []
           else constructRangesRec remainingSymbols rangeDescFiltered

type RangeDesc a b = (Int,Int,[(a,b)])

constructRangesRec :: (Eq a, Eq b, Num b, Show a, Show b) => [a] -> [RangeDesc a b] -> [Ranges]
constructRangesRec a b | trace ("constructRangesRec " ++ show a ++ " " ++ show b) False = undefined
constructRangesRec [] [] = []
constructRangesRec (current:rest) rangeDescs =
        let symbolLoc = findSymbol current rangeDescs
            subwords = calcSubwords symbolLoc
        in [ RangeMap subword restRanges |
             subword <- subwords,
             let newDescs = constructNewRangeDescs rangeDescs symbolLoc subword,
             let restRanges = constructRangesRec rest newDescs
           ]
constructRangesRec [] (_:_) = error "programming error"

findSymbol :: (Eq a, Eq b, Num b) => a -> [RangeDesc a b] -> ((RangeDesc a b,Int),(RangeDesc a b,Int))
findSymbol s rangeDesc =
         let Just (i,j,r)  = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == 1) l') rangeDesc
             Just (m,n,r') = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == 2) l') rangeDesc
             Just a1Idx = elemIndex (s,1) r
             Just a2Idx = elemIndex (s,2) r'
         in (((i,j,r),a1Idx),((m,n,r'),a2Idx))

constructNewRangeDescs :: (Eq a, Eq b) => [RangeDesc a b] -> ((RangeDesc a b,Int),(RangeDesc a b,Int)) -> Subword2 -> [RangeDesc a b]
constructNewRangeDescs descs symbolPositions subword =
        let newDescs = [ newDesc | desc <- descs, newDesc <- processRangeDesc desc symbolPositions subword ]
            count = foldr (\(_,_,l) r -> r + length l) 0
        in assert (count descs > count newDescs) newDescs

processRangeDesc :: (Eq a, Eq b) => RangeDesc a b -> ((RangeDesc a b,Int),(RangeDesc a b,Int)) -> Subword2 -> [RangeDesc a b]
processRangeDesc inp ((left,a1Idx),(right,a2Idx)) (m,n,o,p)
  | inp /= left && inp /= right = [inp]
  | inp == left && inp == right =
        -- at this point it doesn't matter what the actual ordering is
        -- so we just swap if necessary to make it easier for processRangeDescDouble
        let (a1Idx',a2Idx',m',n',o',p') = 
                if a1Idx < a2Idx then
                    (a1Idx,a2Idx,m,n,o,p)
                else
                    (a2Idx,a1Idx,o,p,m,n)
        in processRangeDescDouble inp a1Idx' a2Idx' (m',n',o',p')
  | inp == left = processRangeDescSingle left a1Idx (m,n)
  | inp == right = processRangeDescSingle right a2Idx (o,p)

filterEmptyRanges :: [RangeDesc a b] -> [RangeDesc a b]
filterEmptyRanges l = 
        let f (i,j,d) = not $ null d && i == j
        in filter f l

processRangeDescSingle :: (Eq a, Eq b) => RangeDesc a b -> Int -> Subword -> [RangeDesc a b]
processRangeDescSingle (i,j,r) aIdx (k,l)
  | aIdx == 0 = filterEmptyRanges [(l,j,tail r)]
  | aIdx == length r - 1 = [(i,k,init r)]
  | otherwise = [(i,k,take aIdx r),(l,j,drop (aIdx + 1) r)]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- assumes that a1Idx < a2Idx, see processRangeDesc
processRangeDescDouble :: (Eq a, Eq b) => RangeDesc a b -> Int -> Int -> Subword2 -> [RangeDesc a b]
processRangeDescDouble (i,j,r) a1Idx a2Idx (k,l,m,n)
  -- assert a1Idx < a2Idx, where to put it?!
  | a1Idx == 0 && a2Idx == length r - 1 = filterEmptyRanges [(l,m,init (tail r))]
  | a1Idx == 0 = filterEmptyRanges [(l,m,slice 1 (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]
  | a2Idx == length r - 1 = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r)]
  | otherwise = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]


calcSubwords :: ((RangeDesc a b,Int),(RangeDesc a b,Int)) -> [Subword2]
calcSubwords (left@((i,j,r),a1Idx),right@((m,n,r'),a2Idx))
  | i == m && j == n = calcSubwordsDependent (i,j,r) a1Idx a2Idx
  | length r == 1 && length r' == 1 = [(i,j,m,n)]
  | length r == 1  = [(i',j',k',l') | let (i',j') = (i,j), (k',l') <- calcSubwordsIndependent right]
  | length r' == 1 = [(i',j',k',l') | let (k',l') = (m,n), (i',j') <- calcSubwordsIndependent left]
  | otherwise = [(i',j',k',l') | (i',j') <- calcSubwordsIndependent left, (k',l') <- calcSubwordsIndependent right]

-- assumes that other component is in a different part
calcSubwordsIndependent :: (RangeDesc a b,Int) -> [Subword]
calcSubwordsIndependent ((i,j,r),axIdx)
  | axIdx == 0            = [(k,l) | let k = i, l <- [i..j]]
  | axIdx == length r - 1 = [(k,l) | let l = j, k <- [i..j]]
  | otherwise             = [(k,l) | k <- [i..j], l <- [k..j]] 

-- assumes that other component is in the same part
calcSubwordsDependent :: RangeDesc a b -> Int -> Int -> [Subword2]
calcSubwordsDependent (i,j,r) a1Idx a2Idx =
        let a1Idx' = if a1Idx < a2Idx then a1Idx else a2Idx
            a2Idx' = if a1Idx < a2Idx then a2Idx else a1Idx
            subs = doCalcSubwordsDependent (i,j,r) a1Idx' a2Idx'
        in if a1Idx < a2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]
 
-- assumes that components are in-order (post-processing by calcSubwordsDependent)
doCalcSubwordsDependent :: RangeDesc a b -> Int -> Int -> [Subword2]
doCalcSubwordsDependent (i,j,r) a1Idx a2Idx
  | a1Idx == 0 && a2Idx == length r - 1 = 
        [ (k,l,m,n) | let (k,n) = (i,j), l <- [i..j], m <- [l..j] ]
        
  | a1Idx == 0 =
        [ (k,l,m,n) | let k = i, l <- [i..j], m <- [l..j], n <- [m..j] ]
        
  | a2Idx == length r - 1 =
        [ (k,l,m,n) | let n = j, m <- [i..j], l <- [m..i], k <- [l..i] ]
  
  | a1Idx > 0 && a2Idx < length r - 1 =
        [ (k,l,m,n) | k <- [i..j], l <- [k..j], m <- [l..j], n <- [m..j] ]

  | otherwise = error "invalid state, e.g. a1Idx == a2Idx == 0"
        

       
         
-- 2-dim to 1-dim with 1 tuple
-- not possible yet, see comment at ~~~
instance Rewriting ((a,a) -> [a]) where
  constructRanges f subword = undefined


type Filter2 a = Array Int a -> Subword2 -> Bool
with2 :: Parser2 a b -> Filter2 a -> Parser2 a b
with2 q c z subword = if c z subword then q z subword else []

-- TODO this is a workaround for now as we don't directly support 1-dim yet
-- for this to work, there has to be a dummy start rule like this:
{- s' (c1,c2) = ([c1,c2],[]) -- 1-dim simulated as 2-dim
   s = start <<< k >>> s'
   
   with start r = r in the algebra 
-}
axiom'        :: Int -> Array Int a -> Parser2 a b -> [b]
axiom' l z ax   =  ax z (0,0,0,l)

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
        answer -> answer -> answer,
        answer -> answer
   )

testAlg :: TestAlgebra Char Int
testAlg = (nil, left, f, f2, f3, start) where
   nil () = 0
   left (EPS,c) = ord c
   f (l,r) = ord l + ord r
   f2 (l,r) (l2,r2) = ord l + ord r + ord l2 + ord r2
   f3 l r = l + r
   start r = r
   
testGram :: TestAlgebra Char answer -> String -> [answer]
testGram alg inp = axiom s where
  (nil, left, f, f2, f3, start) = alg
  
  s' :: (a,a) -> ([a],[a])
  s' (c1,c2) = ([],[c1,c2]) -- 1-dim simulated as 2-dim
  
  s = start <<< k >>> s'
  
--  f3' :: (a,a) -> (a,a) -> ([a],[a]) 
--  f3' (p1,p2) (k1,k2) = ([k1,p1],[p2,k2])

  f3' :: [(a,a)] -> ([(a,a)],[(a,a)]) 
  f3' [p1,p2,k1,k2] = ([k1,p1],[p2,k2])

  -- FIXME theoretical problem here, endless recursion
  {- We need to know at least the minimal yield size of p which is 1,1 here.
     Then we can apply this knowledge when building the ranges.
     In a recursion, the range must always get smaller, otherwise we're doomed.
     See section 5.3.3 Yield Size Analysis (Sauthoff, 2011, dissertation).
     
     Either we encode this information as special ~~. combinators or the parsers
     themselves have these information, possibly handed to the top from the simplest
     char parsers. New combinators are probably easier but they allow for user errors.
     
     In our case, combinators might not be possible anyway as the subwords aren't
     generated by the ~~~ combinators itself but in a separate phase through >>>.  
  -}  
  k = f3 <<< p ~~~ k >>> f3' ||| 
      start <<< p >>> f' -- or just p
  
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
  
test = testGram testAlg "agauc"


-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,n) (zip [1..n] xs) where n = length xs