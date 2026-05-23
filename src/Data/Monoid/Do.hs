module Data.Monoid.Do ((>>)) where

import Prelude (Monoid (..))

(>>) :: (Monoid m) => m -> m -> m
(>>) = mappend
