-- file: Main.hs
-- author: Jacob Xie
-- date: 2023/08/22 21:55:01 Tuesday
-- brief:

module Main where

import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
