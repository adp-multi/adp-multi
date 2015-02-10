-- | Convenience module to import the specific rewriting function model
--   and combinator implementation known as /explicit/.
--   In package adp-multi-monadiccp, there is another
--   combinator implementation.
module ADP.Multi.Rewriting.All (module X) where

import ADP.Multi.Rewriting.Model as X
import ADP.Multi.Rewriting.Combinators()