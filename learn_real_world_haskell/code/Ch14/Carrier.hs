{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- file: Carrier.hs
-- author: Jacob Xie
-- date: 2024/04/28 11:19:40 Sunday
-- brief:

module Main where

import Data.Map qualified as M

type PersonName = String

type PhoneNumber = String

type BillingAddress = String

data MobileCarrier
  = Honest_Bobs_Phone_Network
  | Morrisas_Marvelous_Mobiles
  | Petes_Plutocratic_Phones
  deriving (Eq, Ord)

findCarrierBillingAddress ::
  PersonName ->
  M.Map PersonName PhoneNumber ->
  M.Map PhoneNumber MobileCarrier ->
  M.Map MobileCarrier BillingAddress ->
  Maybe BillingAddress
findCarrierBillingAddress = undefined

variation1 :: (Ord k1, Ord k2, Ord k3) => k1 -> M.Map k1 k2 -> M.Map k2 k3 -> M.Map k3 a -> Maybe a
variation1 person phoneMap carrierMap addressMap =
  case M.lookup person phoneMap of
    Nothing -> Nothing
    Just number ->
      case M.lookup number carrierMap of
        Nothing -> Nothing
        Just carrier -> M.lookup carrier addressMap

variation2 :: (Ord k1, Ord k2, Ord k3) => k1 -> M.Map k1 k2 -> M.Map k2 k3 -> M.Map k3 b -> Maybe b
variation2 person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  M.lookup carrier addressMap

variation3 :: (Ord a) => a -> M.Map a a -> M.Map a a -> M.Map a b -> Maybe b
variation3 person phoneMap carrierMap addressMap =
  lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where
    lookup = flip M.lookup

main :: IO ()
main = do
  putStrLn "whatever"
