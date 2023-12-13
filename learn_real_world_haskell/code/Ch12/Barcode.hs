-- file: Barcode.hs
-- author: Jacob Xie
-- date: 2023/11/23 21:12:56 Thursday
-- brief:

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Array (Array (..), bounds, elems, indices, ixmap, listArray, (!))
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Char (digitToInt)
import Data.Function (on)
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

-- Finding matching digits
-- run length encoding

type Run = Int

type RunLength a = [(Run, a)]

runLength :: (Eq a) => [a] -> RunLength a
runLength = map rle . group
  where
    rle xs = (length xs, head xs)

runLengths :: (Eq a) => [a] -> [Run]
runLengths = map fst . runLength

-- scaling run lengths, and finding approximate matches

type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
  where
    divide d = fromIntegral d / divisor
    divisor = fromIntegral (sum xs)

-- A more compact alternative that "knows" we're using Ratio Int:
-- scaleToOne xs = map (% sum xs) xs

type ScoreTable = [[Score]]

-- "SRL" means "scaled run length"
asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList

leftEvenSRL = asSRL leftEvenList

rightSRL = asSRL rightList

paritySRL = asSRL parityList

distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

type Digit = Word8

bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores
  where
    scores = zip [distance d (scaleToOne ps) | d <- srl] digits
    digits = [0 .. 9]

-- remembering a match's parity

data Parity a = Even a | Odd a | None a deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
  fmap = parityMap

-- on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
-- on f g x y = g x `f` g y

compareWithoutParity :: Parity (Score, Digit) -> Parity (Score, Digit) -> Ordering
compareWithoutParity = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps =
  sortBy compareWithoutParity $
    map Odd (bestScores leftOddSRL ps) ++ map Even (bestScores leftEvenSRL ps)

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

-- another kind of laziness, of the keyboarding variety

data AltParity a
  = AltEven {fromAltParity :: a}
  | AltOdd {fromAltParity :: a}
  | AltNone {fromAltParity :: a}
  deriving (Show)

-- chunking a list

chuckWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chuckWith _ [] = []
chuckWith f xs = let (h, t) = f xs in h : chuckWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chuckWith $ splitAt n

-- generating a list of candidate digits

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One) : _) = []
candidateDigits rle | length rle < 59 = []
candidateDigits rle
  | any null match = []
  | otherwise = map (map (fmap snd)) match
  where
    match = map bestLeft left ++ map bestRight right
    left = chunksOf 4 . take 24 . drop 3 $ runLengths
    right = chunksOf 4 . take 24 . drop 32 $ runLengths
    runLengths = map fst rle

-- Turning digit soup into an answer
-- solving for check digits in parallel

type Map a = M.Map Digit [a]

type DigitMap = Map Digit

type ParityMap = Map (Parity Digit)

updateMap ::
  Parity Digit -> -- new digit
  Digit -> -- existing key
  [Parity Digit] -> -- existing digit sequence
  ParityMap -> -- map to update
  ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit : seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m
  where
    key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit = new `M.union` M.foldrWithKey (updateMap digit) M.empty old

incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
-- incorporateDigits old digits = foldl' (useDigit old) M.empty digits
incorporateDigits old = foldl' (useDigit old) M.empty

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 []) . mapEveryOther (map $ fmap (* 3))
