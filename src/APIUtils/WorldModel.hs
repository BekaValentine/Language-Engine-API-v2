{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}







-- | This module defines world models, used to represent the state of the
-- world as the system knows it at any given moment.

module APIUtils.WorldModel where

import Golem.Core.Term
import Golem.Utils.ABT
import Golem.Utils.Names
import Golem.Utils.Plicity

import Control.Monad.State
import Data.Aeson (FromJSON,ToJSON)
import qualified Data.Binary as B
import GHC.Generics







-- | An entity is just an integer, for the internal representation.

type Entity = Int





-- | We can convert an entity into a witnessed term for a discourse context by
-- turning it into an @External@.

entityToWitnessedTerm :: Entity -> (Term,Term)
entityToWitnessedTerm i =
  let a = conH (Absolute "LE" "Entity") []
  in (externalH i a, a)





-- | A 'Fact' is a piece of information about an entity. These are essentially
-- just atomic predicates of the relevant sort.

data Fact
  = PredDesc String Entity
  | RelDesc String Entity Entity
  | UnaryDesc String Entity
  | BinaryStringDesc String Entity String
  deriving (Show,Generic)

instance B.Binary Fact
instance FromJSON Fact
instance ToJSON Fact





-- | We can turn a 'Fact' into a term by convertting to the appropriate con
-- data and externals.

factToTerm :: Fact -> Term
factToTerm (PredDesc p e) =
  conH (Absolute "LE" "Pred")
       [ (Expl,conH (Absolute "LE" p) [])
       , (Expl,externalH e (conH (Absolute "LE" "Entity") []))
       ]
factToTerm (RelDesc r e y) =
  conH (Absolute "LE" "Rel")
       [ (Expl,conH (Absolute "LE" r) [])
       , (Expl,externalH e (conH (Absolute "LE" "Entity") []))
       , (Expl,externalH y (conH (Absolute "LE" "Entity") []))
       ]
factToTerm (UnaryDesc c e) =
  conH (Absolute "LE" c)
       [ (Expl,externalH e (conH (Absolute "LE" "Entity") []))
       ]
factToTerm (BinaryStringDesc c e s) =
  conH (Absolute "LE" c)
       [ (Expl,externalH e (conH (Absolute "LE" "Entity") []))
       , (Expl,In (MkStr s))
       ]





-- | We can turn a 'Fact' into a witnessed term for a discourse context by
-- postulating it.

factToWitnessedTerm :: Fact -> (Term,Term)
factToWitnessedTerm a =
  let a' = factToTerm a
  in (postulateH a', a')





-- | A world model consists of a collection of entities, together with some
-- entity descriptions about them.

data WorldModel
  = WorldModel
    { nextEntity :: Entity
    , facts :: [Fact]
    }
  deriving (Show,Generic)

instance B.Binary WorldModel
instance ToJSON WorldModel





-- | The empty work model is the initial world model with a next entity of 0,
-- and no facts.

emptyWorldModel :: WorldModel
emptyWorldModel =
  WorldModel
  { nextEntity = 0
  , facts = []
  }





-- | We can convert a world model into a set of witnessed terms for a
-- discourse context by turning the entities into external elements and facts
-- into postulates.

worldModelToWitnessedTerms :: WorldModel -> [(Term,Term)]
worldModelToWitnessedTerms (WorldModel nextEnt fcts) =
  map entityToWitnessedTerm [0..nextEnt-1] ++ map factToWitnessedTerm fcts





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