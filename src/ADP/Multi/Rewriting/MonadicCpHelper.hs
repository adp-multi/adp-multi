{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module ADP.Multi.Rewriting.MonadicCpHelper (
        FDModel
      , solveModel
) where

import Control.CP.FD.OvertonFD.OvertonFD
import Control.CP.FD.OvertonFD.Sugar()
import Control.CP.FD.FD (FDIntTerm, getMinimizeVar)
import Control.CP.FD.Model

import Control.CP.FD.Interface
import Control.CP.SearchTree
import Control.CP.EnumTerm
import Control.CP.ComposableTransformers
import Control.CP.FD.Solvers

import ADP.Debug

type FDModel = 
      forall s m. (Show (FDIntTerm s), FDSolver s, MonadTree m, TreeSolver m ~ FDInstance s) 
      => m ModelCol

solveModel :: Tree (FDInstance OvertonFD) ModelCol -> [[Int]]
solveModel f = 
        let (visitedNodes, result) = solve dfs it $ f >>= labeller
        in trace ("FD model solved, nodes visited: " ++ show visitedNodes) result

labeller :: forall s m.
            (Show (FDIntTerm s), EnumTerm s (FDIntTerm s), FDSolver s, MonadTree m, TreeSolver m ~ FDInstance s)
            => ModelCol -> m [TermBaseType s (FDIntTerm s)]
labeller col =
  label $ do
    minVar <- getMinimizeVar
    case minVar of
      Nothing -> return $ labelCol col
      Just v -> return $ do
        enumerate [v]
        labelCol col