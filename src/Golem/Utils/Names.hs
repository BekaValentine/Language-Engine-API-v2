{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}







-- | This module defines some general name constructs that replace raw string
-- names in many variants.

module Golem.Utils.Names where

import Data.Binary
import GHC.Generics







-- | A local name is a name for an item that's been brought into scope usually
-- via module importing. It can be either a bare name, or a dotted name, used
-- for qualified imports. An absolute name is like a dotted local name only it
-- uses the actual name of the module and the item, independent of renaming.

data Name
  = BareLocal String
  | DottedLocal String String
  | Absolute String String
  deriving (Show,Eq,Ord,Generic)

instance Binary Name


showName :: Name -> String
showName (BareLocal n) = n
showName (DottedLocal m n) = m ++ "." ++ n
showName (Absolute m n) = "!" ++ m ++ "." ++ n