{-# OPTIONS -Wall #-}
{-# LANGUAGE ViewPatterns #-}







-- | This module defines how to pretty print a 'Term' representing a syntactic
-- category.

module APIUtils.CategoryPrettyPrinting where

import Golem.Core.Term
import Golem.Utils.ABT
import Golem.Utils.Names
import Golem.Utils.Pretty

import Data.Functor.Identity







data QCategory
  = QCForall String QCategory
  | QCDone Category


prettyQCategory :: QCategory -> String
prettyQCategory qc =
  case vars of
    [] -> prettyCategory bod
    _ -> "forall " ++ unwords vars ++ ". " ++ prettyCategory bod
  where
    accumulateForalls :: QCategory -> ([String],Category)
    accumulateForalls (QCForall x b) =
      let (xs,b') = accumulateForalls b
      in (x:xs,b')
    accumulateForalls (QCDone b) =
      ([],b)
    
    vars :: [String]
    bod :: Category
    (vars,bod) = accumulateForalls qc


data Category
  = Category :\: Category
  | Category :/: Category
  | Category :>: Category
  | OtherCat Term


parensCategory :: Category -> String
parensCategory c@(OtherCat _) = prettyCategory c
parensCategory c = "(" ++ prettyCategory c ++ ")"


prettyCategory :: Category -> String
prettyCategory (a :\: b) =
  parensCategory a ++ " \\ " ++ parensCategory b
prettyCategory (b :/: a) =
  parensCategory a ++ " / " ++ parensCategory b
prettyCategory (a :>: b) =
  parensCategory a ++ " > " ++ parensCategory b
prettyCategory (OtherCat m) =
  pretty m'
  where
    m' = runIdentity (absoluteToBareLocal m)
    
    absoluteToBareLocal :: Term -> Identity Term
    absoluteToBareLocal (Var v) =
      pure (Var v)
    absoluteToBareLocal (In (Con (Absolute _ b) xs)) =
      let xs' =
            traverse
              (\(plic,x) ->
                ((,) plic) <$> underF absoluteToBareLocal x)
              xs
      in (In . Con (BareLocal b)) <$> xs'
    absoluteToBareLocal (In x) =
      In <$> traverse (underF absoluteToBareLocal) x


termToQCategory :: Term -> QCategory
termToQCategory
  (In (Con (Absolute "LE" "QCForall")
           [_
           ,(_,instantiate0 -> In (Lam _ sc))
           ])) =
  let ([n],b) = descope sc
  in QCForall n (termToQCategory b)
termToQCategory
  (In (Con (Absolute "LE" "QCDone")
           [(_,instantiate0 -> b)])) =
  QCDone (termToCategory b)
termToQCategory m
  = error $ "Cannot convert a non-qcategory term to a QCategory: " ++ pretty m


termToCategory :: Term -> Category
termToCategory
  (In (Con (Absolute "LE" "Under")
           [(_,instantiate0 -> a)
           ,(_,instantiate0 -> b)
           ])) =
  termToCategory a :\: termToCategory b
termToCategory
  (In (Con (Absolute "LE" "Over")
           [(_,instantiate0 -> b)
           ,(_,instantiate0 -> a)
           ])) =
  termToCategory b :/: termToCategory a
termToCategory
  (In (Con (Absolute "LE" "Gap")
           [(_,instantiate0 -> a)
           ,(_,instantiate0 -> b)
           ])) =
  termToCategory a :>: termToCategory b
termToCategory m =
  OtherCat m


prettyTermAsQCategory :: Term -> String
prettyTermAsQCategory m =
  prettyQCategory (termToQCategory m)