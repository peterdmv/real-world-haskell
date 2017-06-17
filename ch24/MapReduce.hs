module MapReduce
    (
      mapReduce
    ) where


import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (Strategy, rpar, dot)

-- type Done = ()
-- type Strategy a = a -> Done

-- parList :: Strategy a -> Strategy [a]
-- parList strat []     = ()
-- parList strat (x:xs) = strat x `par` (parList strat xs)

parList :: Strategy a -> Strategy [a]
parList strat = traverse (rpar `dot` strat)

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat


using :: a -> Strategy a -> a
using x s = s x `seq` x


simpleMapReduce :: (a -> b)      -- map function
                -> ([b] -> c)    -- reduce function
                -> [a]           -- list to map over
                -> c
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc


mapReduce :: Strategy b    -- evaluation strategy for mapping
          -> (a -> b)      -- map function
          -> Strategy c    -- evaluation strategy for reduction
          -> ([b] -> c)    -- reduce function
          -> [a]           -- list to map over
          -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat
