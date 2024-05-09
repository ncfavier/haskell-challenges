module Lib
    ( foldr
    ) where

import           Data.Foldable (foldl')
import           Prelude       hiding (foldr)

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr c n t = foldl' (\b a -> b . c a) (\x -> x) t n
