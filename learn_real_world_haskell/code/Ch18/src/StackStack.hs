-- file: StackStack.hs
-- author: Jacob Xie
-- date: 2024/06/13 22:47:50 Thursday
-- brief:

import Control.Monad.Reader
import Control.Monad.State

type Foo = StateT Int (State String)

outerPut :: Int -> Foo ()
outerPut = put

innerPut :: String -> Foo ()
innerPut = lift . put

type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put
