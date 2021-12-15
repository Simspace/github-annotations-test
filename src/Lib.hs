module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = fmap f . fmap g $ pure "Hello world!" where
    f = const ()
    g = id
