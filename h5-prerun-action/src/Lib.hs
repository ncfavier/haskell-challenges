{-# LANGUAGE BlockArguments #-}
module Lib
    ( prerun
    ) where

import Control.Concurrent.MVar
import Control.Monad

prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun f = do
    r <- newEmptyMVar
    b <- f (join (takeMVar r))
    pure \ a -> putMVar r a >> b
