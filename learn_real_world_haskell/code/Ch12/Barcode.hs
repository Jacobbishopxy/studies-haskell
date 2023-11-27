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

-- Encoding an EAN-13 barcode

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

-- this function computes the check digit; don't pass one in.
encodeDigits :: [Int] -> [String]
encodeDigits s@(first : rest) =
  outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where
    (left, right) = splitAt 5 rest
    lefties = zipWith leftEncode (parityCodes ! first) left
    righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard = "101"

centerGuard = "01010"

-- parsing a color image

type Pixel = Word8

type RGB = (Pixel, Pixel, Pixel)

type Pixmap = Array (Int, Int) RGB

parseRawPPM :: Parse Pixmap
parseRawPPM =
  parseWhileWith w2c (/= '\n') ==> \header ->
    skipSpaces
      ==>& assert (header == "P6") "invalid raw header"
      ==>& parseNat
      ==> \width ->
        skipSpaces
          ==>& parseNat
          ==> \height ->
            skipSpaces
              ==>& parseNat
              ==> \maxValue ->
                assert (maxValue == 255) "max value out of spec"
                  ==>& parseByte
                  ==>& parseTimes (width * height) parseRGB
                  ==> \pxs ->
                    identity (listArray ((0, 0), (width - 1, height - 1)) pxs)

parseRGB :: Parse RGB
parseRGB =
  parseByte ==> \r ->
    parseByte ==> \g ->
      parseByte ==> \b ->
        identity (r, g, b)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x :) <$> parseTimes (n - 1) p

-- greyscale conversion

luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r, g, b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b

type Greymap = Array (Int, Int) Pixel

pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance

-- greyscale to binary, and type safety

data Bit = Zero | One deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
  where
    binary i
      | i < pivot = Zero
      | otherwise = One
    pivot = round $ least + (greatest - least) * n
    least = fromIntegral $ choose (<) a
    greatest = fromIntegral $ choose (>) a
    choose f = foldA1 $ \x y -> if f x y then x else y
