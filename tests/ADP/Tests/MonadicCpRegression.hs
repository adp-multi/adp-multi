{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module ADP.Tests.MonadicCpRegression where

import Control.CP.FD.OvertonFD.OvertonFD
import Control.CP.FD.OvertonFD.Sugar()
import Control.CP.FD.FD (FDIntTerm, getMinimizeVar)
import Control.CP.FD.Model

import Control.CP.FD.Interface
import Control.CP.SearchTree
import Control.CP.EnumTerm
import Control.CP.ComposableTransformers
import Control.CP.FD.Solvers


type FDModel = 
      forall s m. (Show (FDIntTerm s), FDSolver s, MonadTree m, TreeSolver m ~ (FDInstance s)) 
      => m ModelCol

model :: FDModel
model = exists $ \col -> do
  [len1,len2] <- colList col 2
  xsum col @= 2
  len1 @>= 0
  len2 @>= 1
  2 @<= 1 
  return col

main :: IO ()
main = print $ solveModel model



-- returns the number of nodes visited and the actual result
-- if there's no solution, an empty list is returned
solveModel :: Tree (FDInstance OvertonFD) ModelCol -> (Int, [[Int]])
solveModel f = solve dfs it $ f >>= labeller

labeller col =
  label $ do
    minVar <- getMinimizeVar
    case minVar of
      Nothing -> return $ labelCol col
      Just v -> return $ do
        enumerate [v]
        labelCol col