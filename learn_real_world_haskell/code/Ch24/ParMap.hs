-- file: ParMap.hs
-- author: Jacob Xie
-- date: 2024/06/30 09:51:05 Sunday
-- brief:

import Control.Parallel

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x : xs) = let r = f x in r `par` r : parallelMap f xs
parallelMap _ _ = []

forceList :: [a] -> ()
forceList (x : xs) = x `pseq` forceList xs
forceList _ = ()

stricterMap :: (a -> b) -> [a] -> [b]
stricterMap f xs = forceList xs `seq` map f xs

forceListAndElts :: (a -> ()) -> [a] -> ()
forceListAndElts forceElt (x : xs) = forceElt x `seq` forceListAndElts forceElt xs
forceListAndElts _ _ = ()
