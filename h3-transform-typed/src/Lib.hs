{-# LANGUAGE GADTs #-}

module Lib where

import           Data.Proxy
import           Data.Typeable

data Scheme a where
    Res :: Typeable a => Proxy a -> Scheme a
    Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)

data Function = forall a. Function (Scheme a) a
data Transform a = forall b. Transform (Scheme b) (a -> b)

newtype Wrap a = Wrap
    { unWrap :: a
    }

transform :: Scheme a -> Transform a
transform (Res _) = Transform (Res Proxy) Wrap
transform (Arg _ s) = case transform s of Transform s' t -> Transform (Arg Proxy s') (\f -> t . f . unWrap)

wrapFunction :: Function -> Function
wrapFunction (Function s f) = case transform s of Transform s' t -> Function s' (t f)
