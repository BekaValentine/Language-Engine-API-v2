{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}







-- | This module defines how to build packages.

module APIUtils.PackageBuilds where

import Golem.Core.Program (Program)
import Golem.Unification.Elaboration
import Golem.Unification.Elaborator

import Data.Binary
import Data.List
import GHC.Generics







-- | A 'PackageBuild' is just a collection of elaboration outputs.

data PackageBuild
  = PackageBuild
    { packageSignature :: Signature
    , packageDefinitions :: Definitions
    , packageModuleNames :: [String]
    , packageOpenData :: [(String,String)]
    , packageOpenFunctions :: [OpenFunction]
    }
  deriving (Generic)

instance Binary PackageBuild





-- | We can elaborate a program to produce a package by elaborating, and then
-- wrapping the appropriate components.

build :: Program -> Either String PackageBuild
build prog =
  do (_,elabState) <- runElaborator0 (elabProgram prog)
     return PackageBuild
            { packageSignature = _signature elabState
            , packageDefinitions = _definitions elabState
            , packageModuleNames = _moduleNames elabState
            , packageOpenData = _openData elabState
            , packageOpenFunctions = _openFunctions elabState
            }





-- | We can elaborate a whole program relative to set of background packages
-- by elaborating relative to the total elaborator state that they produce,
-- and then removing the components that came from the default packages.

buildWithPreludes :: [PackageBuild] -> Program -> Either String PackageBuild
buildWithPreludes bg prog =
  do let sig = concatMap packageSignature bg
         defs = concatMap packageDefinitions bg
         ctx = []
         als = []
         modname = ""
         mods = concatMap packageModuleNames bg
         odata = concatMap packageOpenData bg
         ofun = concatMap packageOpenFunctions bg
     (_,elabState) <- runElaborator
                        (elabProgram prog)
                        sig
                        defs
                        ctx
                        als
                        modname
                        mods
                        odata
                        ofun
     return PackageBuild
            { packageSignature =
                do ((m,n),x) <- _signature elabState
                   case lookup (m,n) sig of
                     Nothing -> return ((m,n),x)
                     Just _ -> []
            , packageDefinitions =
                do ((m,n),x) <- _definitions elabState
                   case lookup (m,n) defs of
                     Nothing -> return ((m,n),x)
                     Just _ -> []
            , packageModuleNames =
                _moduleNames elabState \\ mods
            , packageOpenData =
                _openData elabState \\ odata
            , packageOpenFunctions =
                do ((m,n),x) <- _openFunctions elabState
                   case lookup (m,n) ofun of
                     Nothing -> return ((m,n),x)
                     Just _ -> []
            }