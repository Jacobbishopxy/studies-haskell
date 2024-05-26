-- file: MovieReview.hs
-- author: Jacob Xie
-- date: 2024/05/24 23:48:02 Friday
-- brief:

module MovieReview (module MovieReview) where

import Control.Monad (liftM3)

data MovieReview = MovieReview
  { revTitle :: String,
    revUser :: String,
    revReview :: String
  }

simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_ : _)) ->
      case lookup "user" alist of
        Just (Just user@(_ : _)) ->
          case lookup "review" alist of
            Just (Just review@(_ : _)) ->
              Just (MovieReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- no user
    _ -> Nothing -- no title

maybeReview :: [(String, Maybe [Char])] -> Maybe MovieReview
maybeReview alist = do
  title <- lookup1 "title" alist
  user <- lookup1 "user" alist
  review <- lookup1 "review" alist
  return $ MovieReview title user review

lookup1 :: (Eq a1) => a1 -> [(a1, Maybe [a2])] -> Maybe [a2]
lookup1 key alist =
  case lookup key alist of
    Just (Just s@(_ : _)) -> Just s
    _ -> Nothing

liftedReview :: [(String, Maybe [Char])] -> Maybe MovieReview
liftedReview alist =
  liftM3
    MovieReview
    (lookup1 "title" alist)
    (lookup1 "user" alist)
    (lookup1 "review" alist)
