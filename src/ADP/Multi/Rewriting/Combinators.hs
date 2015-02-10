{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides instance implementations for the >>> combinator
--   using the /explicit/ subword construction algorithm.
module ADP.Multi.Rewriting.Combinators where

import ADP.Multi.Combinators
import ADP.Multi.Rewriting.Model
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.Explicit

instance Rewritable Dim1 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize1 f infos, rewrite constructSubwords1 (infos,p) f)
    
instance Rewritable Dim2 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize2 f infos, rewrite constructSubwords2 (infos,p) f)