{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- file: Logger.hs
-- author: Jacob Xie
-- date: 2024/03/09 09:48:52 Saturday
-- brief:

module Logger
  ( Logger,
    Log,
    runLogger,
    record,
  )
where

import Control.Monad (liftM, liftM2)

----------------------------------------------------------------------------------------------------
-- DAT
----------------------------------------------------------------------------------------------------

type Log = [String]

newtype Logger a = Logger {execLogger :: (a, Log)}

----------------------------------------------------------------------------------------------------

instance Functor Logger where
  fmap f (Logger (a, l)) = Logger (f a, l)

instance Applicative Logger where
  pure a = Logger (a, [])
  (<*>) (Logger (h, w)) (Logger (a, x)) = Logger (h a, w ++ x)

instance Monad Logger where
  m >>= k =
    let (a, w) = execLogger m
        n = k a
        (b, x) = execLogger n
     in Logger (b, w ++ x)

----------------------------------------------------------------------------------------------------
-- fn
----------------------------------------------------------------------------------------------------

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

globToRegex :: String -> Logger String
globToRegex cs =
  globToRegex' cs >>= \ds -> return $ '^' : ds

-- globToRegex' ('?' : cs) =
--   record "any"
--     >> globToRegex' cs
--     >>= \ds -> return ('.' : ds)
-- globToRegex' ('*' : cs) = do
--   record "kleene star"
--   ds <- globToRegex' cs
--   return $ ".*" ++ ds
-- globToRegex' ('[' : '!' : c : cs) =
--   record "character class, negative"
--     >> charClass cs
--     >>= \ds -> return $ "[^" ++ c : ds
-- globToRegex' ('[' : c : cs) =
--   record "character class"
--     >> charClass cs
--     >>= \ds -> return $ "[" ++ c : ds
-- globToRegex' ('[' : _) =
--   fail "unterminated character class"

record :: String -> Logger ()
record s = Logger ((), [s])

-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = m >>= \i -> return $ f i

charClass_wordy (']' : cs) =
  globToRegex' cs >>= \ds -> return $ ']' : ds
charClass_wordy (c : cs) =
  charClass_wordy cs >>= \ds -> return $ c : ds

charClass (']' : cs) = (']' :) `liftM` globToRegex' cs
charClass (c : cs) = (c :) `liftM` globToRegex' cs

globToRegex' (c : cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
  | c `elem` regexChars = record "escape" >> return ['\\', c]
  | otherwise = return [c]
  where
    regexChars = "\\+()^$.{}]|"

-- liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
-- liftM2 f m1 m2 =
--   m1 >>= \a ->
--     m2 >>= \b ->
--       return (f a b)
