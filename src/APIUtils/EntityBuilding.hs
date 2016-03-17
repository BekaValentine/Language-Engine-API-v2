{-# OPTIONS -Wall #-}

{-# LANGUAGE DeriveGeneric #-}







-- | This module defines how to build an entity or series of entity from a
-- type that describes them.

module APIUtils.EntityBuilding where

import Debug
import Golem.Utils.Pretty

import APIUtils.WorldModel
import Golem.Core.RequireSolving (compose)
import Golem.Core.Term
import Golem.Utils.ABT
import Golem.Utils.Names
import Golem.Utils.Telescope

import Control.Applicative
import Control.Monad.State
import Data.Aeson
import GHC.Generics







-- | An entity description is a piece of information about an entity. These
-- are essentially just atomic predicates of the relevant sort.

data EntityDescription
  = PredDesc String Entity
  | RelDesc String Entity Entity
  | UnaryDesc String Entity
  | BinaryStringDesc String Entity String
  deriving (Generic)

instance ToJSON EntityDescription





-- | A 'Construction' is just a stateful process with a 'WorldModel' and a
-- list of 'EntityDescription's as state..

type Construction = StateT (WorldModel,[EntityDescription]) Maybe





-- | We can lift a 'WorldUpdate' to a 'Construction' by running the update on
-- the world model.

liftWorldUpdate :: WorldUpdate a -> Construction a
liftWorldUpdate wu =
  do (wm,eds) <- get
     case runStateT wu wm of
       Nothing -> empty
       Just (a,wm') ->
         do put (wm',eds)
            return a





leCon :: [String]
leCon =
  [ "Pred"
  , "Rel"
  , "Background"
  , "Nonexistant"
  , "StringVal"
  ]
  

-- | We can construct a proof of a type, modifying a world state, provided
-- that the type is a function type, a record type, or a 'Pred' or 'Rel' type
-- condata type. Doing so will yield some 'EntityDescription's.

makeTrue :: WorldModel -> Term -> Maybe (WorldModel,[EntityDescription])
makeTrue wm fact0 =
  do (_,p) <-
       runStateT
         (go (worldModelToTerms wm) fact0)
         (wm,[])
     return p
  where
    go :: [(Term,Term)] -> Term -> Construction Term
    go fcts fact@(In (Fun _ a sc)) =
      do forM_ (compose fcts (instantiate0 a)) $ \m ->
           go fcts (instantiate sc [m])
         return (postulateH fact)
    go _ (In (Con (Absolute "LE" "Entity") [])) =
      do ent <- liftWorldUpdate newEntity
         return (externalH ent (conH (Absolute "LE" "Entity") []))
    go _ fact@(In (Con (Absolute "LE" c) _)) | c `elem` leCon =
      do liftWorldUpdate (assertFact fact)
         return (postulateH fact)
    go fcts (In (RecordType fs (Telescope ascs0))) =
      do debug_ (pretty (In (RecordType fs (Telescope ascs0))))
         xs <- goTele fcts [] ascs0
         return (recordConH (zip fs xs))
    go _ _ = empty
    
    goTele :: [(Term,Term)] -> [Term] -> [Scope TermF] -> Construction [Term]
    goTele _ acc [] =
      do debug_ (unlines (map pretty acc))
         return []
    goTele fcts acc (asc:ascs) =
      do debug_ (unlines (map pretty acc))
         x <- go fcts (instantiate asc acc)
         xs <- goTele fcts (acc ++ [x]) ascs
         return (x:xs)