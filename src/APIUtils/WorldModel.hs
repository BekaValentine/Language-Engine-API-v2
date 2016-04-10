{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}







-- | This module defines world models, used to represent the state of the
-- world as the system knows it at any given moment.

module APIUtils.WorldModel where

import Golem.Core.Term
import Golem.Utils.Names

import Control.Monad.State
import qualified Data.Binary as B
import GHC.Generics







-- | An entity is just an integer, for the internal representation.

type Entity = Int





-- | We can convert an entity into a term by turning it into an @External@.

entityToTerm :: Entity -> (Term,Term)
entityToTerm i =
  let a = conH (Absolute "LE" "Entity") []
  in (externalH i a, a)





-- | A fact is just a proposition that we'll be postulating.

type Fact = Term





-- | We can turn a fact into a proof constructing it's postulate.

factToTerm :: Fact -> (Term,Term)
factToTerm a = (postulateH a, a)





-- | A world model consists of a collection of entities, together with some
-- facts about them.

data WorldModel
  = WorldModel
    { nextEntity :: Entity
    , facts :: [Fact]
    }
  deriving (Generic)

instance B.Binary WorldModel





-- | The empty work model is the initial world model with a next entity of 0,
-- and no facts.

emptyWorldModel :: WorldModel
emptyWorldModel =
  WorldModel
  { nextEntity = 0
  , facts = []
  }





-- | We can convert a world model into a set of typed terms by turning the
-- entities into external elements and facts into postulates.

worldModelToTerms :: WorldModel -> [(Term,Term)]
worldModelToTerms (WorldModel nextEnt fcts) =
  map entityToTerm [0..nextEnt-1] ++ map factToTerm fcts





-- | World updates are just stateful computations over world models.

type WorldUpdate = StateT WorldModel Maybe





-- | We can make new entities by using the next entity number.

newEntity :: WorldUpdate Entity
newEntity =
  do wm <- get
     put wm { nextEntity = nextEntity wm + 1 }
     return (nextEntity wm)





-- | We can assert a fact by adding it to the list of facts.

assertFact :: Fact -> WorldUpdate ()
assertFact f =
  modify $ \wm -> wm { facts = f:facts wm }