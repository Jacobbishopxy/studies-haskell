-- file: Barcode.hs
-- author: Jacob Xie
-- date: 2023/11/23 21:12:56 Thursday
-- brief:

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
  where
    products = mapEveryOther (* 3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])
