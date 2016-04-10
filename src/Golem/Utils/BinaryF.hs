{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}







-- | This module defines a functor version of 'Binary'.

module Golem.Utils.BinaryF where

import Control.Monad
import Data.Binary







class BinaryF f where
  putF :: Binary t => f t -> Put
  getF :: Binary t => Get (f t)



newtype Apply f a =
  Apply { runApply :: f a }

instance (BinaryF f, Binary t) => Binary (Apply f t) where
  put (Apply x) = putF x
  get = liftM Apply getF



putListComposeF :: (BinaryF f, Binary t) => [f t] -> Put
putListComposeF xs = put (fmap Apply xs)

getListComposeF :: forall f t. (BinaryF f, Binary t) => Get [f t]
getListComposeF =
  do xs <- get :: Get [Apply f t]
     return $ map runApply xs