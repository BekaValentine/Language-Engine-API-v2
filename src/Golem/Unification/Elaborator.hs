{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}







-- This module defines the core types of a monadic elaborator.

module Golem.Unification.Elaborator where

import Golem.Utils.Env
import Golem.Utils.Plicity
import Golem.Utils.Unifier
import Golem.Utils.Vars

import Golem.Core.ConSig
import Golem.Core.Term

import qualified Control.Lens as L
import Control.Monad.State







-- The quote level judgment tracks how deeply nested under quotes a term is.

data QLJ a = QLJ a Int
  deriving (Functor)





-- | A signature is a collection of constructors together with their
-- constructor signatures. This is used during type checking and elaboration
-- to define the underlying type theory.

type Signature = [((String,String),ConSig)]





-- | A definition consists of a declared name together with its definition
-- and its type.

type Definitions = [((String,String),(Term,Term))]

definitionsToEnvironment :: Definitions -> Env (String,String) Term
definitionsToEnvironment defs =
  [ (x,m) | (x,(m,_)) <- defs ]





-- | A context contains generated variables together with their display names,
-- and their declared types.

type Context = [(FreeVar,QLJ Term)]





-- | Aliases are just maps from local names to absolute names.

type Aliases = [(Either String (String,String), (String,String))]





-- | Open functions have to story their pattern matching definitions so they
-- can be re-built when new instances are added.

type OpenFunction = ((String,String),(Term,[Plicity],CaseMotive,[Clause]))





-- | Declared reset points are just a collection of reset point names with
-- their declared types.

type ResetPoints = [(String,(Term,Term))]





-- | The definition of the state to be carried by the type checking monad for
-- this particular variant.

data ElabState
  = ElabState
    { _signature :: Signature
    , _definitions :: Definitions
    , _context :: Context
    , _substitution :: Substitution TermF
    , _nextMeta :: MetaVar
    , _aliases :: Aliases
    , _moduleName :: String
    , _moduleNames :: [String]
    , _openData :: [(String,String)]
    , _openFunctions :: [OpenFunction]
    , _quoteLevel :: Int
    , _resetPoints :: ResetPoints
    , _resetPointsInScope :: [String]
    , _shiftsInScope :: [String]
    , _holeContext :: Context
    , _contextsForHoles :: [(FreeVar,Context)]
    }
L.makeLenses ''ElabState


type Elaborator = StateT ElabState (Either String)


type TypeChecker = Elaborator


runElaborator :: Elaborator a
              -> Signature
              -> Definitions
              -> Context
              -> Aliases
              -> String
              -> [String]
              -> [(String,String)]
              -> [OpenFunction]
              -> Either String (a,ElabState)
runElaborator e sig defs ctx als modname mods odata ofuns =
  runStateT
    e
    (ElabState
      sig
      defs
      ctx
      []
      (MetaVar 0)
      als
      modname
      mods
      odata
      ofuns
      0
      []
      []
      []
      []
      [])


runElaborator0 :: Elaborator a -> Either String (a,ElabState)
runElaborator0 e = runElaborator e [] [] [] [] "" [] [] []


when' :: Elaborator a -> Elaborator () -> Elaborator ()
when' e1 e2 = do s <- get
                 case runStateT e1 s of
                   Left _ -> return ()
                   Right _  -> e2