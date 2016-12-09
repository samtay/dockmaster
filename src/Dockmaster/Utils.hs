module Dockmaster.Utils
  ( eitherWrap
  , testM
  ) where

eitherWrap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
eitherWrap f _ (Left a)  = Left $ f a
eitherWrap _ g (Right c) = Right $ g c

testM :: (Monad m) => (a -> m Bool) -> m a -> m (Maybe a)
testM predM mx = do
  test <- mx >>= predM
  x <- mx
  return $ if test then Just x else Nothing
