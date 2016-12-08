module Dockmaster.Utils
  ( eitherWrap
  , testM
  ) where

eitherWrap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
eitherWrap f _ (Left a)  = Left $ f a
eitherWrap _ g (Right c) = Right $ g c

testM :: (Monad m) => (a -> m Bool) -> (a -> b) -> a -> m (Maybe b)
testM predM f x = do
  test <- predM x
  return $ if test then Just (f x) else Nothing
