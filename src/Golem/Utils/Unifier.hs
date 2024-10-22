{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}







-- | This module defines what it means to be a unifying type checker.

module Golem.Utils.Unifier where

import Golem.Utils.ABT
import Golem.Utils.Elaborator
import Golem.Utils.Pretty
import Golem.Utils.Vars

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List (nubBy)







-- * Some general ways of dealing with substitutions for metavariables.



-- | A substitution for a functor is just a metavar lookup for ABTs.

type Substitution f = [(MetaVar, ABT f)]


substMetasSubs :: (Functor f, Foldable f)
               => Substitution f -> Substitution f -> Substitution f
substMetasSubs newSubs subs =
  [ (v', substMetas newSubs t') | (v',t') <- subs ]





-- | Given some way to get the current substitution, we can complete it with
-- new substitutions by substituting their bound values for the new metavars.

completeSubstitution :: (Functor f, Foldable f, MonadState s m)
                     => Lens' s (Substitution f)
                     -> Substitution f
                     -> m ()
completeSubstitution subsl subs' =
  do subs <- getElab subsl
     let subs2 = subs' ++ subs
         subs2' = nubBy (\(a,_) (b,_) -> a == b)
                        (map (\(k,v) -> (k,substMetas subs2 v)) subs2)
     putElab subsl subs2'





-- | Given a way to get contexts and a way to get the current substitution,
--  we can substitute into it with the current substitution.

substituteContexts :: (Eq a, Functor f, Foldable f, MonadState s m)
                   => Lens' s (Substitution f)
                   -> [ReifiedLens' s [(a, ABT f)]]
                   -> m ()
substituteContexts subsl ctxls =
  forM_ ctxls $ \(Lens ctxl) ->
    do ctx <- getElab ctxl
       subs2 <- getElab subsl
       putElab ctxl (map (\(x,t) -> (x,substMetas subs2 t)) ctx)





-- | Given a way to get the context and a way to get the current substitution,
--  we can substitute into it with the current substitution. This version
-- differs from the last in that it uses a functorial judgment in the context
-- instead of just a term. This is necessary for fancier type theories that
-- have judgments other than @A true@.

substituteContextsJ :: (Eq a, Functor f, Foldable f, MonadState s m, Functor j)
                    => Lens' s (Substitution f)
                    -> [ReifiedLens' s [(a, j (ABT f))]]
                    -> m ()
substituteContextsJ subsl ctxls =
  forM_ ctxls $ \(Lens ctxl) ->
    do ctx <- getElab ctxl
       subs2 <- getElab subsl
       putElab ctxl (map (\(x,t) -> (x,fmap (substMetas subs2) t)) ctx)





-- | Given some new substitutions, we can add them to the existing
-- substitutions and in the process update a context-like value.

updateSubstitution :: (Eq a, Functor f, Foldable f, MonadState s m)
                   => Lens' s (Substitution f)
                   -> [ReifiedLens' s [(a,ABT f)]]
                   -> Substitution f
                   -> m ()
updateSubstitution subsl ctxls subs =
  do completeSubstitution subsl subs
     substituteContexts subsl ctxls





-- | Given some new substitutions, we can add them to the existing
-- substitutions and in the process update a context-like value using any
-- functorial judgment.

updateSubstitutionJ :: ( Eq a, Functor f, Foldable f
                       , MonadState s m, Functor j
                       )
                     => Lens' s (Substitution f)
                     -> [ReifiedLens' s [(a,j (ABT f))]]
                     -> Substitution f
                     -> m ()
updateSubstitutionJ subsl ctxls subs =
  do completeSubstitution subsl subs
     substituteContextsJ subsl ctxls
    






-- * Unification by equation solving



-- | Equations are just pairs of values.

data Equation a = Equation a a


-- | Equations can be substituted into, as will be necessary during solving.

substMetasEquation :: (Functor f, Foldable f)
                   => Substitution f -> Equation (ABT f) -> Equation (ABT f)
substMetasEquation subs (Equation l r) =
  Equation (substMetas subs l) (substMetas subs r)





-- | A collection of constructions can be unified providing eq can equate
-- ABTs. This involves producing a new set of equations from the input.

class Monad m => MonadUnify f m where
  equate :: f (Scope f)
         -> f (Scope f)
         -> m [Equation (ABT f)]





-- | Solving a set of equations involves repeatedly equating.

solve :: ( Functor f, Foldable f, Pretty (ABT f)
         , MonadUnify f m, MonadError String m
         )
      => [Equation (ABT f)] -> m (Substitution f)
solve eqs0 = go eqs0 []
  where
    
    go :: ( Functor f, Foldable f, Pretty (ABT f)
          , MonadUnify f m, MonadError String m
          )
       => [Equation (ABT f)] -> Substitution f -> m (Substitution f)
    
    go [] subs = return subs
    
    go (Equation (Var (Meta x)) (Var (Meta y)):eqs) subs
      | x == y = go eqs subs
      | otherwise = go eqs' subs'
      where
        newSubs = [(x, Var (Meta y))]
        eqs' = map (substMetasEquation newSubs) eqs
        subs' = substMetasSubs newSubs (newSubs ++ subs)
    
    go (Equation v1@(Var (Meta x)) r:eqs) subs =
      do unless (not (occurs x r))
           $ throwError $ "Cannot unify because " ++ pretty v1
                       ++ " occurs in " ++ pretty r
         go eqs' subs'
      where
        newSubs = [(x,r)]
        eqs' = map (substMetasEquation newSubs) eqs
        subs' = substMetasSubs newSubs (newSubs ++ subs)
    
    go (Equation l v2@(Var (Meta y)):eqs) subs =
      do unless (not (occurs y l))
           $ throwError $ "Cannot unify because " ++ pretty v2
                       ++ " occurs in " ++ pretty l
         go eqs' subs'
      where
        newSubs = [(y,l)]
        eqs' = map (substMetasEquation newSubs) eqs
        subs' = substMetasSubs newSubs (newSubs ++ subs)
    
    go (Equation v1@(Var x) v2@(Var y):eqs) subs
      | x == y = go eqs subs
      | otherwise = throwError $ "Cannot unify variables " ++ pretty v1
                              ++ " and " ++ pretty v2
    
    go (Equation (In l) (In r):eqs) subs =
      do newEqs <- equate l r
         go (newEqs ++ eqs) subs
    
    go (Equation l r:_) _ =
      throwError $ "Cannot unify " ++ pretty l ++ " and " ++ pretty r





-- | Unification relative to a substitution and a context will solve the
-- relevant equations to produce some new substitutions, and then update
-- the substitutions to ensure that the new substitutions are accounted for.

unify :: ( Eq a, Functor f, Foldable f, Pretty (ABT f)
         , MonadUnify f m, MonadError String m, MonadState s m
         )
      => Lens' s (Substitution f)
      -> [ReifiedLens' s [(a, ABT f)]]
      -> ABT f -> ABT f -> m ()
unify subsl ctxls l r =
  do newSubs <- solve [Equation l r]
     updateSubstitution subsl ctxls newSubs





-- | Unification relative to a substitution and a context will solve the
-- relevant equations to produce some new substitutions, and then update
-- the substitutions to ensure that the new substitutions are accounted for.
-- This version works for arbitrary functorial judgments.

unifyJ :: ( Eq a, Functor f, Foldable f, Pretty (ABT f)
          , MonadUnify f m, MonadError String m, MonadState s m, Functor j
          )
       => Lens' s (Substitution f)
       -> [ReifiedLens' s [(a, j (ABT f))]]
       -> ABT f -> ABT f -> m ()
unifyJ subsl ctxls l r =
  do newSubs <- solve [Equation l r]
     updateSubstitutionJ subsl ctxls newSubs