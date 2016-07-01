{-# OPTIONS -Wall #-}
{-# LANGUAGE ViewPatterns #-}







module APIUtils.PropositionSerialization where

import Golem.Core.Term
import Golem.Utils.ABT
import Golem.Utils.Names
import Golem.Utils.Vars

import Control.Monad (forM_,replicateM)
import Data.Binary
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char







-- | A 'Proposition' is just a special representation for terms that are the
-- meanings of linguistic expressions. The type is used for serialization to
-- send over the network.

data Proposition
  = Background BoundVar
  | Nonexistent BoundVar
  | Pred String BoundVar
  | Rel String BoundVar BoundVar
  | Act String Proposition
  | Speaker BoundVar
  | Addressee BoundVar
  | And Proposition Proposition
  | Sigma Proposition
  | Quant String BoundVar Proposition Proposition
  deriving (Show)

putBoundVar :: BoundVar -> Put
putBoundVar (BoundVar i) =
  putWord8 (fromInteger (toInteger i))

getBoundVar :: Get BoundVar
getBoundVar =
  do i <- getWord8
     return $ BoundVar (fromInteger (toInteger i))

putAscii :: String -> Put
putAscii s =
  do let l = length s
     putWord8 (fromInteger (toInteger l))
     forM_ s $ \c ->
       putWord8 (fromInteger (toInteger (ord c)))
       

getAscii :: Get String
getAscii =
  do l <- get :: Get Int
     ns <- replicateM l getWord8
     return (map (chr . fromInteger . toInteger) ns)

instance Binary Proposition where
  put (Background x) =
    do putWord8 0
       putBoundVar x
  put (Nonexistent x) =
    do putWord8 1
       putBoundVar x
  put (Pred p x) =
    do putWord8 2
       putAscii p
       putBoundVar x
  put (Rel r x y) =
    do putWord8 3
       putAscii r
       putBoundVar x
       putBoundVar y
  put (Act a p) =
    do putWord8 4
       putAscii a
       put p
  put (Speaker x) =
    do putWord8 5
       putBoundVar x
  put (Addressee x) =
    do putWord8 6
       putBoundVar x
  put (And p q) =
    do putWord8 7
       put p
       put q
  put (Sigma p) =
    do putWord8 8
       put p
  put (Quant q e rs sc) =
    do putWord8 9
       putAscii q
       putBoundVar e
       put rs
       put sc
  get =
    do tag <- getWord8
       case tag of
         0 -> Background <$> getBoundVar
         1 -> Nonexistent <$> getBoundVar
         2 -> Pred <$> getAscii <*> getBoundVar
         3 -> Rel <$> getAscii <*> getBoundVar <*> getBoundVar
         4 -> Act <$> getAscii <*> get
         5 -> Speaker <$> getBoundVar
         6 -> Addressee <$> getBoundVar
         7 -> And <$> get <*> get
         8 -> Sigma <$> get
         9 -> Quant <$> getAscii <*> getBoundVar <*> get <*> get
         _ -> error "Deserialization of a Proposition failed."






termToProposition :: Term -> Maybe Proposition
termToProposition
  (In (Con (Absolute "LE" "Background")
           [(_,instantiate0 -> Var (Bound _ x))])) =
  return $ Background x
termToProposition
  (In (Con (Absolute "LE" "Nonexistent")
           [(_,instantiate0 -> Var (Bound _ x))])) =
  return $ Nonexistent x
termToProposition
  (In (Con (Absolute "LE" "Pred")
           [(_,instantiate0 -> In (Con (Absolute _ p) _))
           ,(_,instantiate0 -> Var (Bound _ x))
           ])) =
  return $ Pred p x
termToProposition
  (In (Con (Absolute "LE" "Rel")
           [(_,instantiate0 -> In (Con (Absolute _ r) _))
           ,(_,instantiate0 -> Var (Bound _ x))
           ,(_,instantiate0 -> Var (Bound _ y))
           ])) =
  return $ Rel r x y
termToProposition
  (In (Con (Absolute "LE" "Act")
           [(_,instantiate0 -> In (Con (Absolute _ a) _))
           ,(_,instantiate0 -> p)
           ])) =
  do p' <- termToProposition p
     return $ Act a p'
termToProposition
  (In (Con (Absolute "LE" "Speaker")
           [(_,instantiate0 -> Var (Bound _ x))])) =
  return $ Speaker x
termToProposition
  (In (Con (Absolute "LE" "Addressee")
           [(_,instantiate0 -> Var (Bound _ x))])) =
  return $ Addressee x
termToProposition
  (In (Con (Absolute "LE" "And")
           [(_,instantiate0 -> p)
           ,(_,instantiate0 -> q)
           ])) =
  do p' <- termToProposition p
     q' <- termToProposition q
     return $ And p' q'
termToProposition
  (In (Con (Absolute "LE" "Sigma")
           [(_,instantiate0 -> In (Lam _ (Scope _ _ p)))])) =
  do p' <- termToProposition p
     return $ Sigma p'
termToProposition
  (In (Con (Absolute "LE" "Quant")
           [(_,instantiate0 -> In (Con (Absolute _ q) []))
           ,(_,instantiate0 -> Var (Bound _ x))
           ,(_,instantiate0 -> In (Lam _ (Scope _ _ rs)))
           ,(_,instantiate0 ->
                 In (Lam _ (Scope _ _ (In (Lam _ (Scope _ _ sc))))))
           ])) =
  do rs' <- termToProposition rs
     sc' <- termToProposition sc
     return $ Quant q x rs' sc'
termToProposition _ =
  Nothing






serializeTerm :: Term -> Maybe String
serializeTerm m =
  do p <- termToProposition m
     return $ unpack (encode p)