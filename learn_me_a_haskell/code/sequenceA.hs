-- file: sequenceA.hs
-- author: Jacob Xie
-- date: 2023/08/02 22:10:19 Wednesday
-- brief:

sequenceA' :: (Applicative f) => [f a] -> f [a]
-- sequenceA' [] = pure []
-- sequenceA' (x : xs) = (:) <$> x <*> sequenceA' xs
sequenceA' = foldr (\x -> (<*>) ((:) <$> x)) (pure [])
