-- file: MapReduce.hs
-- author: Jacob Xie
-- date: 2024/06/30 19:10:39 Sunday
-- brief:

module MapReduce
  ( simpleMapReduce,
    mapReduce,
  )
where

import Control.Parallel (pseq)
import Control.Parallel.Strategies (Strategy, parMap, using)

simpleMapReduce ::
  (a -> b) -> -- map function
  ([b] -> c) -> -- reduce function
  [a] -> -- list to map over
  c
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc

mapReduce ::
  Strategy b -> -- evaluation strategy for mapping
  (a -> b) -> -- map function
  Strategy c -> -- evaluation strategy for reduction
  ([b] -> c) -> -- reduce function
  [a] -> -- list to map over
  c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
  mapResult `pseq` reduceResult
  where
    mapResult = parMap mapStrat mapFunc input
    reduceResult = reduceFunc mapResult `using` reduceStrat
