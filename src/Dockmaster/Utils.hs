module Dockmaster.Utils
  ( eitherWrap
  , testM
  ) where

-- | Basically fmap over Either, but allow two functions for each L/R side
--
-- Used for text packing on the error
eitherWrap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
eitherWrap f _ (Left a)  = Left $ f a
eitherWrap _ g (Right c) = Right $ g c

-- | Just a contrived predicate for returning a maybe value within monad context
--
-- Used during the fallback directory structure
testM :: (Monad m) => (a -> m Bool) -> m a -> m (Maybe a)
testM predM mx = do
  test <- mx >>= predM
  x <- mx
  return $ if test then Just x else Nothing
