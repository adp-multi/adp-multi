{-# LANGUAGE FlexibleInstances #-}

{-
Use monadiccp as a finite-domain constraint solver to construct
subwords in a generic way.
-}
module ADP.Multi.Rewriting.ConstraintSolver where

import Control.Exception
import Data.Maybe
import Data.List (find, elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.YieldSize

type Subword = (Int,Int)


instance Rewriting ([(Int,Int)] -> ([(Int,Int)],[(Int,Int)])) where
  constructRanges _ _ b | trace ("constructRanges2 " ++ show b) False = undefined
  constructRanges f infos (i,j,k,l) = 
        undefined
  determineYieldSize _ infos | trace ("determineYieldSize2 " ++ show infos) False = undefined
  determineYieldSize f infos =
        doDetermineYieldSize f infos