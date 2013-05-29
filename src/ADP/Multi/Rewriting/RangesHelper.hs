{-# OPTIONS_HADDOCK hide #-}

-- | Helper methods used for subword construction.
module ADP.Multi.Rewriting.RangesHelper where

import Control.Exception
import Data.List (elemIndex, find)
import qualified Data.Map as Map

import ADP.Debug
import ADP.Multi.Rewriting.Model
import ADP.Multi.Rewriting.YieldSize

-- an attempt to regain some type safety
type Subword1 = (Int,Int)
type Subword2 = (Int,Int,Int,Int)

-- | List of parser symbols and a start and end index over
--   which subwords shall be constructed.
--   Note: RangeDesc means Range Description. I don't like
--         that name very much, but haven't found a good alternative.
type RangeDesc = (Int,Int,[SymbolID])

-- | The list index position of a SymbolID in a RangeDesc.
type SymbolPos = (RangeDesc,Int) 

-- | Searches for the given SymbolID in a list of RangeDesc's
--   and returns its position.  
findSymbol :: SymbolID -> [RangeDesc] -> SymbolPos
findSymbol symId r | trace ("findSymbol " ++ show symId ++ " " ++ show r) False = undefined
findSymbol symId rangeDescs =
         let Just (i,j,r) = find (\(_,_,l) -> symId `elem` l) rangeDescs
             Just symIdx = elemIndex symId r
         in ((i,j,r),symIdx)

findSymbol1 :: Int -> [RangeDesc] -> SymbolPos
findSymbol1 s = findSymbol (s,1)

findSymbol2 :: Int -> [RangeDesc] -> (SymbolPos,SymbolPos)
findSymbol2 s rangeDescs = (findSymbol (s,1) rangeDescs, findSymbol (s,2) rangeDescs)

constructNewRangeDescs1 :: [RangeDesc] -> SymbolPos -> Subword1 -> [RangeDesc]
constructNewRangeDescs1 d p s | trace ("constructNewRangeDescs1 " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs1 descs symbolPosition subword =
        let newDescs = [ newDesc |
                         desc <- descs
                       , newDesc <- processRangeDesc1 desc symbolPosition subword
                       ]
            countSymbols = foldr (\(_,_,l) r -> r + length l) 0
        in assert (countSymbols descs > countSymbols newDescs) $
           trace (show newDescs) $
           newDescs

constructNewRangeDescs2 :: [RangeDesc] -> (SymbolPos,SymbolPos) -> Subword2 -> [RangeDesc]
constructNewRangeDescs2 d p s | trace ("constructNewRangeDescs2 " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs2 descs symbolPositions subword =
        let newDescs = [ newDesc |
                         desc <- descs
                       , newDesc <- processRangeDesc2 desc symbolPositions subword
                       ]
            countSymbols = foldr (\(_,_,l) r -> r + length l) 0
        in assert (countSymbols descs > countSymbols newDescs) $
           trace (show newDescs) $
           newDescs

processRangeDesc1 :: RangeDesc -> SymbolPos -> Subword1 -> [RangeDesc]
processRangeDesc1 a b c | trace ("processRangeDesc1 " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDesc1 inp (desc,symIdx) (m,n)
  | inp /= desc = [inp]
  | otherwise = processRangeDescSingle desc symIdx (m,n)

processRangeDesc2 :: RangeDesc -> (SymbolPos,SymbolPos) -> Subword2 -> [RangeDesc]
processRangeDesc2 a b c | trace ("processRangeDesc2 " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDesc2 inp ((left,sym1Idx),(right,sym2Idx)) (m,n,o,p)
  | inp /= left && inp /= right = [inp]
  | inp == left && inp == right =
        -- at this point it doesn't matter what the actual ordering is
        -- so we just swap if necessary to make it easier for processRangeDescDouble
        let (sym1Idx',sym2Idx',m',n',o',p') =
                if sym1Idx < sym2Idx then
                    (sym1Idx,sym2Idx,m,n,o,p)
                else
                    (sym2Idx,sym1Idx,o,p,m,n)
        in processRangeDescDouble inp sym1Idx' sym2Idx' (m',n',o',p')
  | inp == left = processRangeDescSingle left sym1Idx (m,n)
  | inp == right = processRangeDescSingle right sym2Idx (o,p)

processRangeDescSingle :: RangeDesc -> Int -> Subword1 -> [RangeDesc]
processRangeDescSingle a b c | trace ("processRangeDescSingle " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDescSingle (i,j,r) symIdx (k,l)
  | symIdx == 0 = [(l,j,tail r)]
  | symIdx == length r - 1 = [(i,k,init r)]
  | otherwise = [(i,k,take symIdx r),(l,j,drop (symIdx + 1) r)]

-- assumes that sym1Idx < sym2Idx, see processRangeDesc
processRangeDescDouble :: RangeDesc -> Int -> Int -> Subword2 -> [RangeDesc]
processRangeDescDouble a b c d | trace ("processRangeDescDouble " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
processRangeDescDouble (i,j,r) sym1Idx sym2Idx (k,l,m,n) =
  assert (sym1Idx < sym2Idx) result where
  result | sym1Idx == 0 && sym2Idx == length r - 1 = [(l,m,init (tail r))]
         | sym1Idx == 0 = [(l,m,slice 1 (sym2Idx-1) r),(n,j,drop (sym2Idx+1) r)]
         | sym2Idx == length r - 1 = [(i,k,take sym1Idx r),(l,m,slice (sym1Idx+1) (sym2Idx-1) r)]
         | otherwise = [(i,k,take sym1Idx r),(l,m,slice (sym1Idx+1) (sym2Idx-1) r),(n,j,drop (sym2Idx+1) r)]
    where slice from to xs = take (to - from + 1) (drop from xs)


-- | Returns the yield size of the symbol at the given index in
--   the given RangeDesc. 
yieldSizeOf :: YieldSizeMap -> SymbolPos -> YieldSize
yieldSizeOf yieldSizeMap ((_,_,r),symIdx) =
        -- TODO !! might be expensive as it's a list
        yieldSizeMap Map.! (r !! symIdx)

-- | calculates the combined yield size of all symbols left of the given one
combinedYieldSizeLeftOf :: YieldSizeMap -> SymbolPos -> YieldSize
combinedYieldSizeLeftOf yieldSizeMap (desc,symIdx)
  | symIdx == 0 = (0, Just 0)
  | otherwise =
        let leftYieldSizes = map (\i -> yieldSizeOf yieldSizeMap (desc,i)) [0..symIdx-1]
        in combineYields leftYieldSizes

-- | calculates the combined yield size of all symbols right of the given one
combinedYieldSizeRightOf :: YieldSizeMap -> SymbolPos -> YieldSize
combinedYieldSizeRightOf yieldSizeMap (desc@(_,_,r),symIdx)
  | symIdx == length r - 1 = (0, Just 0)
  | otherwise =
        let rightYieldSizes = map (\i -> yieldSizeOf yieldSizeMap (desc,i)) [symIdx+1..length r - 1]
        in combineYields rightYieldSizes