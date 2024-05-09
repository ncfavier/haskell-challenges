module Lib
    ( shortestLongest
    ) where

import Control.Applicative
import Data.Maybe

steps = go []
  where
    go l (x:xs) = Nothing : go (x:l) xs
    go l [] = Just [reverse l] : repeat (Just [])

traverseThen f g = fmap f . traverse g

shortestLongest :: [[[a]]] -> [[a]]
shortestLongest = head
                . catMaybes
                . getZipList
                . traverseThen mconcat (traverseThen (fmap concat . sequenceA)
                                                     (ZipList . steps))
