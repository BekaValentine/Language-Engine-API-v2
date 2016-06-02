{-# OPTIONS -Wall #-}

{-# LANGUAGE DeriveGeneric #-}







-- | This module defines how to build an entity or series of entity from a
-- type that describes them.

module APIUtils.EntityBuilding where

import qualified APIUtils.WorldModel as WM
import Golem.Core.RequireSolving (compose)
import Golem.Core.Term
import Golem.Utils.ABT
import Golem.Utils.Names
import Golem.Utils.Telescope

import Control.Applicative
import Control.Monad.State







-- | A 'Construction' is just a stateful process with a 'WorldModel' and a
-- list of 'EntityDescription's as state..

type Construction = StateT (WM.WorldModel,[WM.Fact]) Maybe





-- | We can lift a 'WorldUpdate' to a 'Construction' by running the update on
-- the world model.

liftWorldUpdate :: WM.WorldUpdate a -> Construction a
liftWorldUpdate wu =
  do (wm,eds) <- get
     case runStateT wu wm of
       Nothing -> empty
       Just (a,wm') ->
         do put (wm',eds)
            return a





-- | We can make an entity by lifting the world model update.

newEntity :: Construction WM.Entity
newEntity = liftWorldUpdate WM.newEntity





-- | We can assert an entity description by adding it to the state.

assertFact :: WM.Fact -> Construction ()
assertFact f =
  do liftWorldUpdate (WM.assertFact f)
     (wm,fs) <- get
     put (wm,f:fs)





-- | We can construct a proof of a type, modifying a world state, provided
-- that the type is a function type, a record type, or a 'Pred' or 'Rel' type
-- condata type. Doing so will yield some 'EntityDescription's.

makeTrue :: WM.WorldModel -> Term -> Maybe (WM.WorldModel,[WM.Fact])
makeTrue wm fact0 =
  do (_,p) <-
       runStateT
         (go (WM.worldModelToWitnessedTerms wm) fact0)
         (wm,[])
     return p
  where
    go :: [(Term,Term)] -> Term -> Construction Term
    go fcts fact@(In (Fun _ a sc)) =
      do forM_ (compose fcts (instantiate0 a)) $ \m ->
           go fcts (instantiate sc [m])
         return (postulateH fact)
    go _ (In (Con (Absolute "LE" "Entity") [])) =
      do ent <- newEntity
         return (externalH ent (conH (Absolute "LE" "Entity") []))
    go _ fact@(In (Con (Absolute "LE" "Pred") [(_,psc),(_,esc)])) =
      do In (Con (Absolute "LE" p) []) <- return (instantiate0 psc)
         In (External e _) <- return (instantiate0 esc)
         assertFact (WM.PredDesc p e)
         return (postulateH fact)
    go _ fact@(In (Con (Absolute "LE" "Rel") [(_,rsc),(_,esc),(_,xsc)])) =
      do In (Con (Absolute "LE" r) []) <- return (instantiate0 rsc)
         In (External e _) <- return (instantiate0 esc)
         In (External x _) <- return (instantiate0 xsc)
         assertFact (WM.RelDesc r e x)
         return (postulateH fact)
    go _ fact@(In (Con (Absolute "LE" "Background") [(_,esc)])) =
      do In (External e _) <- return (instantiate0 esc)
         assertFact (WM.UnaryDesc "Background" e)
         return (postulateH fact)
    go _ fact@(In (Con (Absolute "LE" "Nonexistant") [(_,esc)])) =
      do In (External e _) <- return (instantiate0 esc)
         assertFact (WM.UnaryDesc "Nonexistant" e)
         return (postulateH fact)
    go _ fact@(In (Con (Absolute "LE" "StringVal") [(_,esc),(_,strsc)])) =
      do In (External e _) <- return (instantiate0 esc)
         In (MkStr s) <- return (instantiate0 strsc)
         assertFact (WM.BinaryStringDesc "StringVal" e s)
         return (postulateH fact)
    go fcts (In (RecordType fs (Telescope ascs0))) =
      do xs <- goTele fcts [] ascs0
         return (recordConH (zip fs xs))
    go _ _ = empty
    
    goTele :: [(Term,Term)] -> [Term] -> [Scope TermF] -> Construction [Term]
    goTele _ _ [] =
      return []
    goTele fcts acc (asc:ascs) =
      do x <- go fcts (instantiate asc acc)
         xs <- goTele fcts (acc ++ [x]) ascs
         return (x:xs)