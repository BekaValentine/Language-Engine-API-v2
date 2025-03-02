{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}







module Golem.Utils.Vars where

import Data.Aeson
import Data.Binary
import GHC.Generics







-- * Variable types



-- A free variable is just a 'String' but we use a @newtype@ to prevent
-- accidentally using it for the wrong things.

newtype FreeVar = FreeVar String
  deriving (Show,Eq,Ord,Generic)

instance Binary FreeVar
instance ToJSON FreeVar



-- A bound variable is just an 'Int' but we use a @newtype@ to prevent
-- accidentally using it for the wrong things.

newtype BoundVar = BoundVar Int
  deriving (Show,Eq,Ord,Generic)

instance Binary BoundVar
instance ToJSON BoundVar



-- | A meta variable is just an 'Int' but we use a @newtype@ to prevent
-- accidentally using it for the wrong things.

newtype MetaVar = MetaVar Int
  deriving (Show,Num,Eq,Ord,Generic)

instance Binary MetaVar
instance ToJSON MetaVar







-- * Freshening names



-- | We can freshen a set of names relative to some other names. This ensures
-- that the freshened names are distinct from the specified names, and also
-- distinct from one another.

freshen :: [String] -> [String] -> [String]
freshen others ns = reverse (go (reverse ns))
  where
    go :: [String] -> [String]
    go [] = []
    go (oldN:oldNs) = newN:newNs
      where
        newNs = go oldNs
        newN = freshenName (newNs ++ others) oldN


-- | We can freshen a single name relative to some other set of names,
-- ensuring that the new name is distinct from all the specified names.

freshenName :: [String] -> String -> String
freshenName others n
  | n == "_" = n
  | n `elem` others = freshenName others (n ++ "'")
  | otherwise = n