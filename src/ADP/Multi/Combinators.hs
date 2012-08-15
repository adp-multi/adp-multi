{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-} -- needed for Parseable

module ADP.Multi.Combinators where
import Debug.Trace
import Control.Exception
import Data.Maybe
import Data.Array
import Data.Char (ord)
import Data.List (find, elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map

-- # Lexical parsers

type Subword  = (Int,Int)
type Subword2  = (Int,Int,Int,Int)
type Parser2 a b = Array Int a -> Subword2 -> [b]

data ParserInfo2 = ParserInfo2 { minYield :: (Int,Int), maxYield :: (Maybe Int,Maybe Int) }
                        deriving Show
type RichParser2 a b = (ParserInfo2, Parser2 a b)


data EPS = EPS

data Ranges = RangeMap Subword2 [Ranges] deriving Show

empty2 :: RichParser2 a ()
empty2 = (
              ParserInfo2 {minYield=(0,0), maxYield=(Just 0,Just 0)},
              \ _ (i,j,k,l) -> [() | i == j && k == l]
         )

anychars :: RichParser2 a (a,a)
anychars = (
                ParserInfo2 {minYield=(1,1), maxYield=(Just 1,Just 1)},
                \ z (i,j,k,l) -> [(z!j, z!l) | i+1 == j && k+1 == l]
           )

chars :: Eq a => a -> a -> RichParser2 a (a,a)
chars c1 c2 = (
                  ParserInfo2 {minYield=(1,1), maxYield=(Just 1,Just 1)},
                  \ z (i,j,k,l) -> [(z!j, z!l) | i+1 == j && k+1 == l && z!j == c1 && z!l == c2]
              ) 
        
charLeftOnly :: Eq a => a -> RichParser2 a (a,EPS)
charLeftOnly c = (
                     ParserInfo2 {minYield=(1,0), maxYield=(Just 1,Just 0)},
                     \ z (i,j,k,l) -> [(c, EPS) | i+1 == j && k == l && z!j == c]
                 )

charRightOnly :: Eq a => a -> RichParser2 a (EPS,a)
charRightOnly c = (
                      ParserInfo2 {minYield=(0,1), maxYield=(Just 0,Just 1)},
                      \ z (i,j,k,l) -> [(EPS, c) | i == j && k+1 == l && z!l == c]
                  )


class Parseable p a b | p -> a b where
    toParser :: p -> RichParser2 a b
    
instance Parseable (RichParser2 a b) a b where
    toParser p = p

instance Parseable () a () where
    toParser _ = empty2 

instance Eq a => Parseable (a,a) a (a,a) where
    toParser (c1,c2) = chars c1 c2
    
instance Eq a => Parseable (EPS,a) a (EPS,a) where
    toParser (_,c) = charRightOnly c
    
instance Eq a => Parseable (a,EPS) a (a,EPS) where
    toParser (c,_) = charLeftOnly c

-- # Parser combinators

combineMinYields :: (Int,Int) -> (Int,Int) -> (Int,Int)
combineMinYields (min11,min12) (min21,min22) = (min min11 min21, min min12 min22)

combineMaxYields :: (Maybe Int,Maybe Int) -> (Maybe Int,Maybe Int) -> (Maybe Int,Maybe Int)
combineMaxYields (a,b) (c,d) =
        ( if isNothing a || isNothing c then Nothing else max a c
        , if isNothing b || isNothing d then Nothing else max b d
        )

infixr 5 ||| 
(|||) :: RichParser2 a b -> RichParser2 a b -> RichParser2 a b
(|||) (ParserInfo2 {minYield=minY1, maxYield=maxY1}, r) (ParserInfo2 {minYield=minY2, maxYield=maxY2}, q) = 
        (
              ParserInfo2 {minYield=combineMinYields minY1 minY2, maxYield=combineMaxYields maxY1 maxY2},
              \ z (i,j,k,l) -> r z (i,j,k,l) ++ q z (i,j,k,l)
        )

infix 8 <<<
(<<<) :: Parseable p a b => (b -> c) -> p -> ([ParserInfo2], [Ranges] -> Parser2 a c)
(<<<) f parseable =
            let (info,parser) = toParser parseable
            in (
                 [info],
                 \ [] z subword -> map f (parser z subword)                                  
            )

-- TODO the dimension of the resulting parser should result from c
infix 6 >>>
(>>>) :: Rewriting c => ([ParserInfo2], [Ranges] -> Parser2 a b) -> c -> RichParser2 a b
--(>>>) _ _ _ a | trace (">>> " ++ show a) False = undefined
(>>>) (infos,p) f =
        let yieldSize = determineYieldSize f infos
        in trace (">>> yield size: " ++ show yieldSize) $
           (
              yieldSize,
              \ z subword ->
                let ranges = constructRanges f infos subword
                in trace (">>> " ++ show subword) $
                trace ("ranges: " ++ show ranges) $ 
                [ result |
                  RangeMap sub rest <- ranges
                , result <- p rest z sub 
                ]
           )  

-- TODO parsers of different dim's should be mixable
-- this seems hard to do ATM, but isn't really a problem for now as lower dimensions can be
-- simulated by higher dimensions by leaving some tuple elements of the rewriting rules empty
infixl 7 ~~~
(~~~) :: Parseable p a b => ([ParserInfo2], [Ranges] -> Parser2 a (b -> c)) -> p -> ([ParserInfo2], [Ranges] -> Parser2 a c)
(~~~) (infos,leftParser) parseable =
        let (info,rightParser) = toParser parseable
        in (
                info : infos,
                \ ranges z subword -> [ pr qr | qr <- rightParser z subword, RangeMap sub rest <- ranges, pr <- leftParser rest z sub ]
           )
                             
     
-- special version of ~~~ which ignores the right parser for determining the yield sizes
-- this must be used for self-recursion
-- To make it complete, there should also be a special version of <<< but this isn't strictly
-- necessary as this can also be solved by not using left-recursion.
-- I guess this only works because of laziness (ignoring the info value of toParser).
infixl 7 ~~~|
(~~~|) :: Parseable p a b => ([ParserInfo2], [Ranges] -> Parser2 a (b -> c)) -> p -> ([ParserInfo2], [Ranges] -> Parser2 a c)
(~~~|) (infos,leftParser) parseable =
        let (_,rightParser) = toParser parseable
            info = ParserInfo2 { minYield=(0,0), maxYield=(Nothing,Nothing) }
        in (
                info : infos,
                \ ranges z subword -> [ pr qr | qr <- rightParser z subword, RangeMap sub rest <- ranges, pr <- leftParser rest z sub ]
           )


class Rewriting f where
  constructRanges :: f -> [ParserInfo2] -> Subword2 -> [Ranges]
  determineYieldSize :: f -> [ParserInfo2] -> ParserInfo2 

-- 2-dim to 2-dim with 1 tuple
instance (Num a, Eq a) => Rewriting ((a,a) -> ([a],[a])) where
  constructRanges _ [info] b | trace ("constructRanges1 " ++ show b ++ " " ++ show info) False = undefined
  constructRanges f [info] (i,j,k,l) =  
     case f (1,2) of
        ([1],[2]) -> [RangeMap (i,j,k,l) []]
        ([2],[1]) -> [RangeMap (k,l,i,j) []]
        ([1,2],[]) -> [RangeMap (i,x,x,j) [] | k == l, x <- [i + fst (minYield info) .. j - snd (minYield info)]]
        ([2,1],[]) -> [RangeMap (x,j,i,x) [] | k == l, x <- [i + fst (minYield info) .. j - snd (minYield info)]]
        ([],[1,2]) -> [RangeMap (k,x,x,l) [] | i == j, x <- [k + fst (minYield info) .. l - snd (minYield info)]]
        ([],[2,1]) -> [RangeMap (x,k,l,x) [] | k == l, x <- [k + fst (minYield info) .. l - snd (minYield info)]]
        _ -> error "invalid rewriting function, each argument must appear exactly once"
  determineYieldSize _ infos | trace ("determineYieldSize1 " ++ show infos) False = undefined
  determineYieldSize f [info @ ParserInfo2 { minYield=(minY1,minY2), maxYield=(maxY1,maxY2) }] =
     case f (1,2) of
        ([1],[2]) -> info
        ([2],[1]) -> ParserInfo2 { minYield = (minY2,minY1), maxYield = (maxY2,maxY1) }
        ([_,_],[]) -> ParserInfo2 { 
                minYield = (minY1+minY2,0),
                maxYield = (if isNothing maxY1 || isNothing maxY2 
                            then Nothing
                            else Just (fromJust maxY1 + fromJust maxY2), Just 0)
                      }
        ([],[_,_]) -> ParserInfo2 { 
                minYield = (0,minY1+minY2),
                maxYield = (Just 0,
                            if isNothing maxY1 || isNothing maxY2 
                            then Nothing
                            else Just $ fromJust maxY1 + fromJust maxY2) }
        _ -> error "invalid rewriting function, each argument must appear exactly once"   

-- 2-dim to 2-dim  with many tuples
instance Rewriting ([(Int,Int)] -> ([(Int,Int)],[(Int,Int)])) where
  constructRanges _ _ b | trace ("constructRanges2 " ++ show b) False = undefined
  constructRanges f infos (i,j,k,l) =
        assert (i <= j && j <= k && k <= l) $
        let parserCount = length infos
            args = concatMap (\ x -> [(x,1),(x,2)]) [1..parserCount] 
            (left,right) = f args
            remainingSymbols = [parserCount,parserCount-1..1]
            rangeDesc = [(i,j,left),(k,l,right)]
            rangeDescFiltered = filterEmptyRanges rangeDesc
        in if any (\(m,n,d) -> null d && m /= n) rangeDesc then []
           else constructRangesRec (buildInfoMap infos) remainingSymbols rangeDescFiltered
  determineYieldSize _ infos | trace ("determineYieldSize2 " ++ show infos) False = undefined
  determineYieldSize f infos =
        let parserCount = length infos
            args = concatMap (\ x -> [(x,1),(x,2)]) [1..parserCount] 
            (left,right) = f args
            elemInfo = buildInfoMap infos
            leftYields = map (\(i,j) -> elemInfo Map.! (i,j)) left
            rightYields = map (\(i,j) -> elemInfo Map.! (i,j)) right
            (leftMin,leftMax) = combineYields leftYields
            (rightMin,rightMax) = combineYields rightYields 
        in trace (show elemInfo) $
           trace (show left) $
           trace (show right) $
           ParserInfo2 { 
                minYield = (leftMin,rightMin),
                maxYield = (leftMax,rightMax)
           }

combineYields :: [Info] -> Info
combineYields = foldl1 $ \(minY1,maxY1) (minY2,maxY2) ->
                    ( minY1+minY2
                    , if isNothing maxY1 || isNothing maxY2 
                      then Nothing
                      else Just $ fromJust maxY1 + fromJust maxY2
                    ) 

type YieldSizes = (Int,Maybe Int) -- min and max yield sizes
type Info = YieldSizes -- at the moment just yield sizes
type InfoMap = Map (Int,Int) Info

-- the input list is in reverse order, i.e. the first in the list is the last applied parser
buildInfoMap :: [ParserInfo2] -> InfoMap
buildInfoMap i | trace ("buildInfoMap " ++ show i) False = undefined
buildInfoMap infos =
        let parserCount = length infos
            list = concatMap (\ (x,info) -> 
                       [ ((x,1), (fst $ minYield info, fst $ maxYield info) ) 
                       , ((x,2), (snd $ minYield info, snd $ maxYield info) ) 
                       ]
                     ) $ zip [parserCount,parserCount-1..] infos
        in Map.fromList list

type RangeDesc = (Int,Int,[(Int,Int)])

{- FIXME
At the moment we get into an infinite recursion because in a rule S -> P S
the ranges for the right S are constructed independently of the min yield size of P 
they also produce an identical one as in the input.


The solution is to look at the symbols left or right to the current
symbol and use their min yield sizes. This would also produce less ranges.

-}


constructRangesRec :: InfoMap -> [Int] -> [RangeDesc] -> [Ranges]
constructRangesRec a b c | trace ("constructRangesRec " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
constructRangesRec _ [] [] = []
constructRangesRec infoMap (current:rest) rangeDescs =
        let symbolLoc = findSymbol current rangeDescs
            subwords = calcSubwords infoMap symbolLoc
        in [ RangeMap subword restRanges |
             subword <- subwords,
             let newDescs = constructNewRangeDescs rangeDescs symbolLoc subword,
             let restRanges = constructRangesRec infoMap rest newDescs
           ]
constructRangesRec _ [] (_:_) = error "programming error"

findSymbol :: Int -> [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int))
findSymbol s r | trace ("findSymbol " ++ show s ++ " " ++ show r) False = undefined
findSymbol s rangeDesc =
         let Just (i,j,r)  = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == 1) l') rangeDesc
             Just (m,n,r') = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == 2) l') rangeDesc
             Just a1Idx = elemIndex (s,1) r
             Just a2Idx = elemIndex (s,2) r'
         in (((i,j,r),a1Idx),((m,n,r'),a2Idx))

constructNewRangeDescs :: [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int)) -> Subword2 -> [RangeDesc]
constructNewRangeDescs d p s | trace ("constructNewRangeDescs " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs descs symbolPositions subword =
        let newDescs = [ newDesc | desc <- descs, newDesc <- processRangeDesc desc symbolPositions subword ]
            count = foldr (\(_,_,l) r -> r + length l) 0
        in assert (count descs > count newDescs) $
           trace (show newDescs) newDescs

processRangeDesc :: RangeDesc -> ((RangeDesc,Int),(RangeDesc,Int)) -> Subword2 -> [RangeDesc]
processRangeDesc a b c | trace ("processRangeDesc " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
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

filterEmptyRanges :: [RangeDesc] -> [RangeDesc]
filterEmptyRanges l = 
        let f (i,j,d) = not $ null d && i == j
        in filter f l

processRangeDescSingle :: RangeDesc -> Int -> Subword -> [RangeDesc]
processRangeDescSingle a b c | trace ("processRangeDescSingle " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDescSingle (i,j,r) aIdx (k,l)
  | aIdx == 0 = filterEmptyRanges [(l,j,tail r)]
  | aIdx == length r - 1 = [(i,k,init r)]
  | otherwise = [(i,k,take aIdx r),(l,j,drop (aIdx + 1) r)]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- assumes that a1Idx < a2Idx, see processRangeDesc
processRangeDescDouble :: RangeDesc -> Int -> Int -> Subword2 -> [RangeDesc]
processRangeDescDouble a b c d | trace ("processRangeDescDouble " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
processRangeDescDouble (i,j,r) a1Idx a2Idx (k,l,m,n) = 
  assert (a1Idx < a2Idx) result where 
  result | a1Idx == 0 && a2Idx == length r - 1 = filterEmptyRanges [(l,m,init (tail r))]
         | a1Idx == 0 = filterEmptyRanges [(l,m,slice 1 (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]
         | a2Idx == length r - 1 = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r)]
         | otherwise = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]

calcSubwords :: InfoMap -> ((RangeDesc,Int),(RangeDesc,Int)) -> [Subword2]
calcSubwords a b | trace ("calcSubwords " ++ show a ++ " " ++ show b) False = undefined
calcSubwords infoMap (left@((i,j,r),a1Idx),right@((m,n,r'),a2Idx))
  | i == m && j == n = calcSubwordsDependent infoMap (i,j,r) a1Idx a2Idx
  | length r == 1 && length r' == 1 = [(i,j,m,n)]
  | length r == 1  = [ (i',j',k',l') |
                        let (i',j') = (i,j)
                     , (k',l') <- calcSubwordsIndependent infoMap right
                     ]
  | length r' == 1 = [ (i',j',k',l') |
                       let (k',l') = (m,n)
                     , (i',j') <- calcSubwordsIndependent infoMap left
                     ]
  | otherwise = [ (i',j',k',l') |
                  (i',j') <- calcSubwordsIndependent infoMap left
                , (k',l') <- calcSubwordsIndependent infoMap right
                ]

infoFromPos :: InfoMap -> (RangeDesc,Int) -> Info
infoFromPos infoMap ((_,_,r),aIdx) | trace ("infoFromPos " ++ show aIdx ++ " " ++ show r ++ " " ++ show infoMap) False = undefined
infoFromPos infoMap ((_,_,r),aIdx) =
        infoMap Map.! (r !! aIdx)
        
-- calculates the combined yield size of all symbols left of the given one
combinedInfoLeftOf :: InfoMap -> (RangeDesc,Int) -> Info
combinedInfoLeftOf infoMap (desc,axIdx)
  | axIdx == 0 = (0, Just 0)
  | otherwise = 
        let leftInfos = map (\i -> infoFromPos infoMap (desc,i)) [0..axIdx-1]
        in combineYields leftInfos

-- calculates the combined yield size of all symbols right of the given one        
combinedInfoRightOf :: InfoMap -> (RangeDesc,Int) -> Info
combinedInfoRightOf infoMap (desc@(_,_,r),axIdx)
  | axIdx == length r - 1 = (0, Just 0)
  | otherwise = 
        let rightInfos = map (\i -> infoFromPos infoMap (desc,i)) [axIdx+1..length r - 1]
        in combineYields rightInfos
        
-- assumes that other component is in a different part
calcSubwordsIndependent :: InfoMap -> (RangeDesc,Int) -> [Subword]
calcSubwordsIndependent a b | trace ("calcSubwordsIndependent " ++ show b) False = undefined
calcSubwordsIndependent infoMap pos@((i,j,r),axIdx) 
  | axIdx == 0            = [(k,l) | let k = i, l <- [i+minY..j-minYRight]]
  | axIdx == length r - 1 = [(k,l) | let l = j, k <- [i+minYLeft..j-minY]]
  | otherwise             = [(k,l) | k <- [i+minYLeft..j-minY], l <- [k+minY..j-minYRight]]
  where (minY,_) = infoFromPos infoMap pos
        (minYLeft,_) = combinedInfoLeftOf infoMap pos
        (minYRight,_) = combinedInfoRightOf infoMap pos

-- assumes that other component is in the same part
calcSubwordsDependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
calcSubwordsDependent _ b c d | trace ("calcSubwordsDependent " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
calcSubwordsDependent infoMap (i,j,r) a1Idx a2Idx =
        let a1Idx' = if a1Idx < a2Idx then a1Idx else a2Idx
            a2Idx' = if a1Idx < a2Idx then a2Idx else a1Idx
            subs = doCalcSubwordsDependent infoMap (i,j,r) a1Idx' a2Idx'
        in if a1Idx < a2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]
 
doCalcSubwordsDependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
doCalcSubwordsDependent infoMap desc@(i,j,r) a1Idx a2Idx =
   assert (a1Idx < a2Idx) result where
   result | a1Idx == 0 && a2Idx == length r - 1 = 
                [ (k,l,m,n) |
                  let (k,n) = (i,j)
                , l <- [i+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2] 
                ]
                
          | a1Idx == 0 =
                [ (k,l,m,n) |
                  let k = i
                , l <- [i+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2-minYRight2]
                , n <- [m+minY2..j-minYRight2]
                ]
                
          | a2Idx == length r - 1 =
                [ (k,l,m,n) |
                  let n = j
                , m <- [i+minYLeft2..j-minY2]
                , l <- [i+minY1+minYLeft1..m-minYBetween]
                , k <- [i+minYLeft1..l-minY1]
                ]
          
          | a1Idx > 0 && a2Idx < length r - 1 =
                [ (k,l,m,n) |
                  k <- [i+minYLeft1..j-minY1-minYRight1]
                , l <- [k+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2-minYRight2]
                , n <- [m+minY2..j-minYRight2]
                ]
        
          | otherwise = error "invalid conditions, e.g. a1Idx == a2Idx == 0"
          where 
                (minY1,_) = infoFromPos infoMap (desc,a1Idx)
                (minY2,_) = infoFromPos infoMap (desc,a2Idx)
                (minYLeft1,_) = combinedInfoLeftOf infoMap (desc,a1Idx)
                (minYLeft2,_) = combinedInfoLeftOf infoMap (desc,a2Idx)
                (minYRight1,_) = combinedInfoRightOf infoMap (desc,a1Idx)
                (minYRight2,_) = combinedInfoRightOf infoMap (desc,a2Idx)
                minYBetween = minYRight1 - minYRight2 - minY2
       
         
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
axiom'        :: Int -> Array Int a -> RichParser2 a b -> [b]
axiom' l z (_,ax) =  ax z (0,0,0,l)

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

  f3' :: [(Int,Int)] -> ([(Int,Int)],[(Int,Int)]) 
  f3' [p1,p2,k1,k2] = ([k1,p1],[p2,k2])

  k = f3 <<< p ~~~| k >>> f3' ||| 
      start <<< p >>> f' -- or just p
  
  f' :: (a,a) -> ([a],[a]) 
  f' (c1,c2) = ([c1],[c2]) -- "identical" function
  p = --tabulated (
      f <<< ('a', 'u') >>> f' |||
      f <<< ('u', 'a') >>> f' |||
      f <<< ('c', 'g') >>> f' |||
      f <<< ('g', 'c') >>> f' |||
      f <<< ('g', 'u') >>> f' |||
      f <<< ('u', 'g') >>> f' 
      --)
      
  test = f   <<< ('a', 'u') >>> f' |||
         nil <<< ()         >>> f' |||
         left <<< (EPS, 'u') >>> f'
      
  -- or using our basepair parser constructed with a filter:
  
--  p2 = f <<< basepair >>> f'

  z         = mk inp
  (_,n)     = bounds (z)
  
  tabulated = table2 n
{-
  basepair  = anychars `with2` basepairing 
  basepairing :: Filter2 Char
  basepairing z (i,j,k,l) = i+1 == j && k+1 == l && isBasepair (z!(i+1), z!(k+1))
  -}
  axiom     = axiom' n z
  
isBasepair ('a','u') = True
isBasepair ('u','a') = True
isBasepair ('c','g') = True
isBasepair ('g','c') = True
isBasepair ('g','u') = True
isBasepair ('u','g') = True
isBasepair _         = False
  
test = testGram testAlg "guaugc"


-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,length xs) (zip [1..] xs)