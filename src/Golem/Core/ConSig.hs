{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}







module Golem.Core.ConSig where

import Golem.Core.DeclArg
import Golem.Core.Term
import Golem.Utils.ABT
import Golem.Utils.Names
import Golem.Utils.Plicity
import Golem.Utils.Pretty (pretty)
import Golem.Utils.Telescope

import Data.Binary
import GHC.Generics







data ConSig = ConSig [Plicity] (BindingTelescope (Scope TermF))
  deriving (Generic)

instance Binary ConSig


instance Show ConSig where
  show (ConSig plics (BindingTelescope ascs bsc)) =
    binders ++ " " ++ pretty (body bsc)
    where
      binders =
        unwords
          (zipWith
             (\n (plic,a) -> wrap plic (n ++ " : " ++ a))
             ns
             (zip plics as))
      as = map (pretty.body) ascs
      ns = names bsc
      
      wrap Expl x = "(" ++ x ++ ")"
      wrap Impl x = "{" ++ x ++ "}"


conSigH :: [DeclArg] -> Term -> ConSig
conSigH declas b = ConSig plics (bindingTelescopeH xs as b)
  where (plics,xas) = unzip [ (plic,(x,a)) | DeclArg plic x a <- declas ]
        (xs,as) = unzip xas


freeToDefinedConSig :: ConSig -> ConSig
freeToDefinedConSig (ConSig plics tele) =
  ConSig plics (fmap (freeToDefinedScope (In . Defined . BareLocal)) tele)