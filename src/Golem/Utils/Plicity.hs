{-# OPTIONS -Wall #-}



-- | A type that represents plicity, i.e. implicit and explicit.
module Golem.Utils.Plicity where



data Plicity = Expl | Impl
  deriving (Show,Eq,Ord)