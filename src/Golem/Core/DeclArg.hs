{-# OPTIONS -Wall #-}



module Golem.Core.DeclArg where

import Golem.Utils.Plicity
import Golem.Utils.Pretty

import Golem.Core.Term



data DeclArg = DeclArg Plicity String Term

instance Show DeclArg where
  show (DeclArg Expl x t) = "(" ++ x ++ " : " ++ pretty t ++ ")"
  show (DeclArg Impl x t) = "{" ++ x ++ " : " ++ pretty t ++ "}"