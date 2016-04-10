{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}







-- | A type that represents plicity, i.e. implicit and explicit.
module Golem.Utils.Plicity where

import Data.Binary
import GHC.Generics







data Plicity = Expl | Impl
  deriving (Show,Eq,Ord,Generic)

instance Binary Plicity