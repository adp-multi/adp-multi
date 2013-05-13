-- | multiple context free grammar,
-- with CYK table parser. (Johannes Waldmann, HTWK Leipzig)

-- Note (Maik): it is actually an Unger-style parser,
--              or: top-down memoizing dynamic programming algorithm
module MCFG.MCFG where

import qualified Data.Map as M
import Control.Monad.State.Strict
import Data.List ( inits, tails )

-- | * data type for grammar

data Nonterminal = N { arity :: Int, name :: String }
    deriving ( Eq, Ord, Show )

newtype Terminal = T Char
    deriving ( Eq, Ord, Show )

data MCFG = MCFG
          { start :: Nonterminal
          , rules :: [ Rule ] }
    deriving ( Eq, Ord, Show )

data Rule = Rule
          { lhs :: Nonterminal
          , function :: [[ Either (Int,Int) Terminal ]]
          , rhs :: [ Nonterminal ]
          }
    deriving ( Eq, Ord, Show )

-- | * data types for derivation trees

data Derivation =
    Derivation { result :: [[ Terminal ]]
               , apply :: Rule
               , children :: [ Derivation ]
               }
    deriving ( Show )

consistent :: Derivation -> Bool
consistent d = 
       and ( map consistent $ children d )
    && result d == do 
          xs <- function ( apply d )
          return $ do
              x <- xs
              case x of
                  Right t -> [ t ]
                  Left (i,j) -> 
                      result ( children d !! i ) !! j

-- | * CYK tabled parser


parse :: MCFG -> [ Terminal ] -> [ Derivation ]
parse g w = fst $ cyk g w


-- | speichert jeweils alle Ableitungen.
-- später dann: die beste Ableitung und ihre Kosten.
type Table = M.Map ( Nonterminal, [[Terminal]] ) 
                   [ Derivation ]

-- | Die Tabelle ist nur polynomiell groß, weil die 
-- Schlüssel aus Teilwörtern der Eingabe bestehen

cyk :: MCFG -> [ Terminal ] -> ( [Derivation], Table )
cyk g w = flip runState M.empty 
          $ build (rules g) (start g , [w] )


build :: [ Rule ] 
      -> ( Nonterminal, [[Terminal]] )
      -> State Table [ Derivation ]
build rules (var, ws) = do
    t <- get
    case M.lookup ( var, ws ) t of
        Just r -> return r
        Nothing -> do
            dss <- sequence $ do
                r <- rules 
                guard $ var == lhs r 
                sub <- forM ( zip ( function r ) ws ) splits
                let c :: M.Map ( Int,Int ) [Terminal]
                    c = M.fromListWith ( error "not linear") 
                      $ concat sub
                return $ do
                    css <- forM ( zip (rhs r ) [0..] ) 
                          $ \ (var' @ (N a _), i) -> do
                              let ws' = for [ 0 .. a-1 ] $ \ j -> c M.! (i,j)
                              build rules ( var' , ws' )
                    return $ for ( combine css ) $ \  cs -> 
                             Derivation { result = ws, apply = r, children = cs }
            let ds :: [ Derivation ]
                ds = concat  dss
            modify $ M.insert (var, ws) ds
            return ds

splits  :: Eq b 
        => ([Either a b], [b])     
        -> [[(a, [b])]]
splits (f, w) = case (f,w) of
    ( [], [] ) -> return []
    (Right x : f', y : w') | x == y -> 
        splits (f', w') 
    (Left a : f', _) -> do
        ( pre, post ) <- zip ( inits w ) ( tails w )
        later <- splits (f', post)
        return $ ( a , pre ) : later
    _ -> []

for :: [a] -> (a -> b) -> [b]
for = flip map

combine :: [[a]] -> [[a]]
combine = foldr ( \ xs ys -> do x <- xs ; y <- ys ; return $ x : y ) [[]] 