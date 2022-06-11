{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Lib
    ( someFunc
    ) where

import Data.Functor.Compose (Compose(..))
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup ((<>))
import Prelude
import qualified Data.Monoid

someFunc :: IO ()
someFunc = fmap f . fmap g $ pure "Hello world!" where
    f = const ()
    g = id

someOtherFunc :: a -> a
someOtherFunc = id $ id
