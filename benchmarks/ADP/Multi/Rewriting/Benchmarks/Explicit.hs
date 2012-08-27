
module ADP.Multi.Rewriting.Benchmarks.Explicit where

import Data.Char (toLower)
import Data.List

import qualified ADP.Tests.RGExample as RG

-- https://github.com/neothemachine/rna/wiki/Example
testRgSimpleCompleteness =
    RG.rgknot RG.enum "agcgu"