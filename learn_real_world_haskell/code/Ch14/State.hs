-- file: State.hs
-- author: Jacob Xie
-- date: 2024/05/03 22:02:39 Friday
-- brief:

module State
  (
  )
where

newtype State s a = State
  { runState :: s -> (a, s)
  }

returnState :: a -> State s a
returnState a = State (a,)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s ->
  let (a, s') = runState m s
   in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)
