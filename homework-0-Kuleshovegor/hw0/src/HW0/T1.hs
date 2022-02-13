{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( assocEither
  , assocPair
  , distrib
  , flipIso
  , nextEither
  , nextPair
  , reverseEither
  , reversePair
  , runIso
  , type (<->) (Iso)
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

nextPair :: (a, (b, c)) -> ((a, b), c)
nextPair (a, (b, c)) = ((a, b), c)

reversePair :: ((a, b), c) -> (a, (b, c))
reversePair ((a, b), c) = (a, (b, c))

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso nextPair reversePair

nextEither :: Either a (Either b c) -> Either (Either a b) c
nextEither (Left a)          = Left $ Left a
nextEither (Right (Left b))  = Left $ Right b
nextEither (Right (Right c)) = Right c

reverseEither :: Either (Either a b) c -> Either a (Either b c)
reverseEither (Left (Left a))  = Left a
reverseEither (Left (Right b)) = Right $ Left b
reverseEither (Right c)        = Right $ Right c

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso nextEither reverseEither
