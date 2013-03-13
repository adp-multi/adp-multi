{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module ADP.Tests.MonadicCpTest where

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
  [x1,x2] <- colList col 2
  allin col (cte 0,cte 8)
  x1 + x2 @= 8
  x1 @>= 1
  x2 @>= 2
  x1 @<= 10
  x2 @<= 12
  -2 @<= x2
  -4 @<= x1
  x1 @<= 8 -- each unnecessary inequality leads to one more visited node 
  x2 @<= 8
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