-- file: Barcode.hs
-- author: Jacob Xie
-- date: 2023/11/23 21:12:56 Thursday
-- brief:

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Array (Array (..), bounds, elems, indices, ixmap, listArray, (!))
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Char (digitToInt)
import Data.Ix (Ix (..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Map qualified as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import Parse
import System.Environment (getArgs)

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
  where
    products = mapEveryOther (* 3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

--

leftOddList =
  [ "0001101",
    "0011001",
    "0010011",
    "0111101",
    "0100011",
    "0110001",
    "0101111",
    "0111011",
    "0110111",
    "0001011"
  ]

rightList = map complement <$> leftOddList
  where
    complement '0' = '1'
    complement '1' = '0'

leftEvenList = map reverse rightList

parityList =
  [ "111111",
    "110100",
    "110010",
    "110001",
    "101100",
    "100110",
    "100011",
    "101010",
    "101001",
    "100101"
  ]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l - 1) xs
  where
    l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

-- folding over arrays

-- Strict left fold, similar to foldl' on lists
foldA :: (Ix k) => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
  where
    go s (j : js) = let s' = f s (a ! j) in s' `seq` go s' js
    go s _ = s

-- Strict left fold using the first element of the array as its starting value, similar to fold1 on lists
foldA1 :: (Ix k) => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a
