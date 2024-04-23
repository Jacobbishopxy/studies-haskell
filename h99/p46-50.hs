-- file: p46-50.hs
-- author: Jacob Xie
-- date: 2024/02/02 21:20:15 Friday
-- brief:

import Control.Monad (replicateM)

-- P46: Truth tables for logical expressions
and', or', nor', nand', xor', impl', equ' :: Bool -> Bool -> Bool
and' = (&&)
or' = (||)
nor' = (not .) . and' -- nor' a b = not (or' a b)
nand' = (not .) . or' -- nand' a b = not (and' a b)
xor' = (not .) . equ' -- xor'  a b = not (equ' a b)
impl' = or' . not -- or' a b = (not a) b
equ' = (==)

myTable :: (Bool -> Bool -> Bool) -> IO ()
myTable f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]

-- P47: Truth tables for logical expressions (part 2)
infixl 4 `or'`

infixl 4 `nor'`

infixl 5 `xor'`

infixl 6 `and'`

infixl 6 `nand'`

infixl 3 `equ'`

-- P48: Truth tables for logical expressions (part 3)
myTableN :: Int -> ([Bool] -> Bool) -> IO ()
myTableN n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
  where
    args n' = replicateM n' [True, False]
    toStr = unwords . map (\x -> show x ++ space x)
    space True = "  "
    space False = " "

-- P49: Gray codes
myGray :: (Integral a) => a -> [String]
myGray 0 = [""]
myGray n = foldr (\s acc -> ("0" ++ s) : ("1" ++ s) : acc) [] $ myGray (n - 1)

-- P50: Huffman codes
myHuffman :: [(Char, Int)] -> [(Char, String)]
-- TODO
myHuffman = undefined
