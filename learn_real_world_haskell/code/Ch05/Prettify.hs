-- file: Prettify.hs
-- author: Jacob Xie
-- date: 2023/08/23 19:05:30 Wednesday
-- brief:

module Prettify where

data Doc = ToBeDefined deriving (Show)

-- string :: String -> Doc
-- string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

(<+>) :: Doc -> Doc -> Doc
a <+> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d <+> p) : punctuate p ds
