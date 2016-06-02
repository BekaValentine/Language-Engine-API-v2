{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}







-- | This module defines the terms of the dependently typed lambda calculus.

module Golem.Core.Term where

import Golem.Utils.ABT
import Golem.Utils.BinaryF
import Golem.Utils.Names
import Golem.Utils.Plicity
import Golem.Utils.Pretty
import Golem.Utils.Telescope

import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Binary
import Data.Bitraversable
import Data.Functor.Classes
import Data.Functor.Compose
import Data.List (intercalate)







-- | Terms in this variant are the same as the Modular variant, except that
-- we also now have terms for record types, record terms, and record
-- projection.
--
-- The term @External n a@ is used to represent references to proof-system
-- external evidence, identified by the integer @n@. That is to say, if we
-- have @External n a@, then we're trusting that there is some mechanism in
-- the host system to provide @a@-like behavior, using @n@ to locate it.
--
-- Similarly, @Postulate a@ is used to represent proof-system external axioms
-- which ought to have no real behavior other than to exist.
--
-- Neither @External@ nor @Postulate@ should be accessible to the user, they
-- exist entirely to permit linking up with non-Golem systems and form a kind
-- of FFI for the language.

data TermF r
  = Defined Name
  | Ann r r
  | Str
  | MkStr String
  | Type
  | Fun Plicity r r
  | Lam Plicity r
  | App Plicity r r
  | Con Name [(Plicity,r)]
  | Case [r] (CaseMotiveF r) [ClauseF r]
  | RecordType [String] (Telescope r)
  | RecordCon [(String,r)]
  | RecordProj r String
  | QuotedType [String] r
  | Quote r
  | Unquote r
  | Continue r
  | Shift String r
  | Reset String r
  | Require r r
  | External Int r
  | Postulate r
  deriving (Functor,Foldable,Traversable)


instance Eq1 TermF where
  eq1 (Defined n) (Defined n') =
    n == n'
  eq1 (Ann m t) (Ann m' t') =
    m == m' && t == t'
  eq1 Str Str =
    True
  eq1 (MkStr s) (MkStr s') =
    s == s'
  eq1 Type Type =
    True
  eq1 (Fun plic a sc) (Fun plic' a' sc') =
    plic == plic' && a == a' && sc == sc'
  eq1 (Lam plic sc) (Lam plic' sc') =
    plic == plic' && sc == sc'
  eq1 (App plic f x) (App plic' f' x') =
    plic == plic' && f == f' && x == x'
  eq1 (Con c ms) (Con c' ms') =
    c == c' && ms == ms'
  eq1 (Case ms motive clauses) (Case ms' motive' clauses') =
    ms == ms' && eq1 motive motive'
      && length clauses == length clauses'
      && all (uncurry eq1) (zip clauses clauses')
  eq1 (RecordType fields tele) (RecordType fields' tele') =
    fields == fields' && eq1 tele tele'
  eq1 (RecordCon fs) (RecordCon fs') =
    fs == fs'
  eq1 (RecordProj m x) (RecordProj m' x') =
    x == x' && m == m'
  eq1 (QuotedType resets a) (QuotedType resets' a') =
    resets == resets' && a == a'
  eq1 (Quote m) (Quote m') =
    m == m'
  eq1 (Unquote m) (Unquote m') =
    m == m'
  eq1 (Continue m) (Continue m') =
    m == m'
  eq1 (Shift res m) (Shift res' m') =
    res == res' && m == m'
  eq1 (Reset res m) (Reset res' m') =
    res == res' && m == m'
  eq1 (Require a sc) (Require a' sc') =
    a == a' && sc == sc'
  eq1 (External i a) (External i' a') =
    i == i' && a == a'
  eq1 (Postulate a) (Postulate a') =
    a == a'
  eq1 _ _ =
    False


data TermFConName
  = DEFINED | ANN | STR | MKSTR | TYPE | FUN | LAM | APP | CON | CASE
  | RECORDTYPE | RECORDCON | RECORDPROJ | QUOTEDTYPE | QUOTE | UNQUOTE
  | CONTINUE | SHIFT | RESET | REQUIRE
  | EXTERNAL | POSTULATE
  deriving (Eq,Ord)

instance Ord1 TermF where
  compare1 (Defined x) (Defined x') =
    compare x x'
  compare1 (Ann m t) (Ann m' t') =
    compare m m' `mappend` compare t t'
  compare1 Str Str = EQ
  compare1 (MkStr s) (MkStr s') =
    compare s s'
  compare1 Type Type = EQ
  compare1 (Fun plic a b) (Fun plic' a' b') =
    compare plic plic' `mappend` compare a a' `mappend` compare b b'
  compare1 (Lam plic m) (Lam plic' m') =
    compare plic plic' `mappend` compare m m'
  compare1 (App plic f x) (App plic' f' x') =
    compare plic plic' `mappend` compare f f' `mappend` compare x x'
  compare1 (Con c ms) (Con c' ms') =
    compare c c' `mappend` compare ms ms'
  compare1 (Case ms mot cls) (Case ms' mot' cls') =
    compare ms ms' `mappend`
    compare1 mot mot' `mappend`
    compare1 (Compose cls) (Compose cls')
  compare1 (RecordType fs as) (RecordType fs' as') =
    compare fs fs' `mappend` compare1 as as'
  compare1 (RecordCon fields) (RecordCon fields') =
    compare fields fields'
  compare1 (RecordProj m x) (RecordProj m' x') =
    compare x x' `mappend` compare m m'
  compare1 (QuotedType rs a) (QuotedType rs' a') =
    compare rs rs' `mappend` compare a a'
  compare1 (Quote m) (Quote m') =
    compare m m'
  compare1 (Unquote m) (Unquote m') =
    compare m m'
  compare1 (Continue m) (Continue m') =
    compare m m'
  compare1 (Shift r m) (Shift r' m') =
    compare r r' `mappend` compare m m'
  compare1 (Reset r m) (Reset r' m') =
    compare r r' `mappend` compare m m'
  compare1 (Require a m) (Require a' m') =
    compare a a' `mappend` compare m m'
  compare1 (External i a) (External i' a') =
    compare i i' `mappend` compare a a'
  compare1 (Postulate a) (Postulate a') =
    compare a a'
  compare1 x y = compare (conName x) (conName y)
    where
      conName :: TermF a -> TermFConName
      conName (Defined _) = DEFINED
      conName (Ann _ _) = ANN
      conName Str = STR
      conName (MkStr _) = MKSTR
      conName Type = TYPE
      conName (Fun _ _ _) = FUN
      conName (Lam _ _) = LAM
      conName (App _ _ _) = APP
      conName (Con _ _) = CON
      conName (Case _ _ _) = CASE
      conName (RecordType _ _) = RECORDTYPE
      conName (RecordCon _) = RECORDCON
      conName (RecordProj _ _) = RECORDPROJ
      conName (QuotedType _ _) = QUOTEDTYPE
      conName (Quote _) = QUOTE
      conName (Unquote _) = UNQUOTE
      conName (Continue _) = CONTINUE
      conName (Shift _ _) = SHIFT
      conName (Reset _ _) = RESET
      conName (Require _ _) = REQUIRE
      conName (External _ _) = EXTERNAL
      conName (Postulate _) = POSTULATE


instance BinaryF TermF where
  putF (Defined n) =
    do put (0 :: Word8)
       put n
  putF (Ann m t) =
    do put (1 :: Word8)
       put m
       put t
  putF Str =
    put (2 :: Word8)
  putF (MkStr s) =
    do put (3 :: Word8)
       put s
  putF Type =
    put (4 :: Word8)
  putF (Fun plic a sc) =
    do put (5 :: Word8)
       put plic
       put a
       put sc
  putF (Lam plic sc) =
    do put (6 :: Word8)
       put plic
       put sc
  putF (App plic f x) =
    do put (7 :: Word8)
       put plic
       put f
       put x
  putF (Con c ms) =
    do put (8 :: Word8)
       put c
       put ms
  putF (Case ms mot cls) =
    do put (9 :: Word8)
       put ms
       put mot
       put cls
  putF (RecordType fs tele) =
    do put (10 :: Word8)
       put fs
       put tele
  putF (RecordCon fs) =
    do put (11 :: Word8)
       put fs
  putF (RecordProj m x) =
    do put (12 :: Word8)
       put m
       put x
  putF (QuotedType res a) =
    do put (13 :: Word8)
       put res
       put a
  putF (Quote m) =
    do put (14 :: Word8)
       put m
  putF (Unquote m) =
    do put (15 :: Word8)
       put m
  putF (Continue m) =
    do put (16 :: Word8)
       put m
  putF (Shift res m) =
    do put (17 :: Word8)
       put res
       put m
  putF (Reset res m) =
    do put (18 :: Word8)
       put res
       put m
  putF (Require a sc) =
    do put (19 :: Word8)
       put a
       put sc
  putF (External i a) =
    do put (20 :: Word8)
       put i
       put a
  putF (Postulate a) =
    do put (21 :: Word8)
       put a
  getF =
    do tag <- getWord8
       case tag of
         0 -> liftM Defined get
         1 -> liftM2 Ann get get
         2 -> return Str
         3 -> liftM MkStr get
         4 -> return Type
         5 -> liftM3 Fun get get get
         6 -> liftM2 Lam get get
         7 -> liftM3 App get get get
         8 -> liftM2 Con get get
         9 -> liftM3 Case get get get
         10 -> liftM2 RecordType get get
         11 -> liftM RecordCon get
         12 -> liftM2 RecordProj get get
         13 -> liftM2 QuotedType get get
         14 -> liftM Quote get
         15 -> liftM Unquote get
         16 -> liftM Continue get
         17 -> liftM2 Shift get get
         18 -> liftM2 Reset get get
         19 -> liftM2 Require get get
         20 -> liftM2 External get get
         21 -> liftM Postulate get
         _ -> undefined



type Term = ABT TermF


-- | A case motive is a telescope that describes the arguments of a case
-- expression and the expression as a whole. Because this variant is
-- dependently typed, the type of a whole case expression can depend on the
-- particular values given to it, so something similar to a @Pi@ type is
-- necessary. For more on the use of motives with case expressions, you can
-- look at Agda's motive-based case function (tho importantly, Agda makes
-- pattern matching functions primary, and case is defined in terms of this).
-- For a more general look at motives, McBride's Elimination with a Motive is
-- a very good resource.

newtype CaseMotiveF r = CaseMotive (BindingTelescope r)
  deriving (Functor,Foldable,Traversable)


instance Eq1 CaseMotiveF where
  eq1 (CaseMotive tele) (CaseMotive tele') =
    eq1 tele tele'


instance Ord1 CaseMotiveF where
  compare1 (CaseMotive tele) (CaseMotive tele') =
    compare1 tele tele'


instance Binary r => Binary (CaseMotiveF r) where
  put (CaseMotive tele) =
    put tele
  get =
    liftM CaseMotive get


type CaseMotive = CaseMotiveF (Scope TermF)


data ClauseF r
  = Clause [PatternF r] r
  deriving (Functor,Foldable,Traversable)


instance Eq1 ClauseF where
  eq1 (Clause ps sc) (Clause ps' sc') =
    length ps == length ps'
      && all (uncurry eq1) (zip ps ps')
      && sc == sc'


instance Ord1 ClauseF where
  compare1 (Clause ps sc) (Clause ps' sc') =
    compare1 (Compose ps) (Compose ps') `mappend` compare sc sc'


instance Binary r => Binary (ClauseF r) where
  put (Clause ps m) =
    do put ps
       put m
  get =
    liftM2 Clause get get


type Clause = ClauseF (Scope TermF)


-- | Patterns in the dependent variant have to be able to have scope all of
-- their own, because they can be binders, but they also need to have
-- assertion terms which are themselves capable of being binders. Because of
-- this, we need to define pattern constructions with a doubly-parameterized
-- type. The @r@ parameter will be used for the recursive occurrences of
-- patterns inside patterns, while the @a@ parameter will be used similar to
-- the @a@ parameter in a 'Fix'-based definition of 'List':
--
-- @
--    data ListF a r = Nil | Cons a r
--
--    type List a = Fix (ListF a)
-- @
--
-- However, the fixed/ABTed pattern will itself be used as a kind of shape,
-- just like lists, 'ClauseF', 'CaseMotiveF', etc. are used above. So the type
-- 'PatternFF' is the shapes of pattern shapes, perversely.

data PatternFF a r
  = ConPat Name [(Plicity,r)]
  | AssertionPat a
  | MakeMeta
  deriving (Functor,Foldable,Traversable)


instance Eq a => Eq1 (PatternFF a) where
  eq1 (ConPat c ps) (ConPat c' ps') =
    c == c' && ps == ps'
  eq1 (AssertionPat m) (AssertionPat m') =
    m == m'
  eq1 MakeMeta MakeMeta =
    True
  eq1 _ _ =
    False


instance Ord a => Ord1 (PatternFF a) where
  compare1 (ConPat c ps) (ConPat c' ps') =
    compare c c' `mappend` compare ps ps'
  compare1 (ConPat _ _) _ = LT
  compare1 (AssertionPat _) (ConPat _ _) = GT
  compare1 (AssertionPat m) (AssertionPat m') =
    compare m m'
  compare1 (AssertionPat _) _ = LT
  compare1 MakeMeta MakeMeta = EQ
  compare1 MakeMeta _ = GT


instance Bifunctor PatternFF where
  bimap _ g (ConPat s xs) = ConPat s [ (plic,g x) | (plic,x) <- xs ]
  bimap f _ (AssertionPat x) = AssertionPat (f x)
  bimap _ _ MakeMeta = MakeMeta


instance Bifoldable PatternFF where
  bifoldMap _ g (ConPat _ xs) = foldMap (g.snd) xs
  bifoldMap f _ (AssertionPat x) = f x
  bifoldMap _ _ MakeMeta = mempty


instance Bizippable PatternFF where
  bizip (ConPat c rs0) (ConPat c' rs0')
    | c == c' && length rs0 == length rs0' =
      let (plics,rs) = unzip rs0
          (plics',rs') = unzip rs0'
      in if (plics == plics')
            then Just ([], zip rs rs')
            else Nothing
    | otherwise = Nothing
  bizip (AssertionPat a) (AssertionPat a') =
    Just ([(a,a')], [])
  bizip MakeMeta MakeMeta = Just ([],[])
  bizip _ _ = Nothing


instance Bitraversable PatternFF where
  -- Older versions of bifunctors required either @bitraverse@ or
  -- @bisequenceA@ for a minimal definition, but since v5.1 this doesn't seem
  -- to be the case. I've left the implementation for @bisequenceA@ intact
  -- but commented out just in case.
  {-
  bisequenceA (ConPat c ps) =
    ConPat c <$> sequenceA [ (,) plic <$> p
                           | (plic,p) <- ps
                           ]
  bisequenceA (AssertionPat m) =
    AssertionPat <$> m
  bisequenceA MakeMeta =
    pure MakeMeta
  -}


instance Binary a => BinaryF (PatternFF a) where
  putF (ConPat c ps) =
    do put (0 :: Word8)
       put c
       put ps
  putF (AssertionPat x) =
    do put (1 :: Word8)
       put x
  putF MakeMeta =
    put (2 :: Word8)
  getF =
    do tag <- getWord8
       case tag of
         0 -> liftM2 ConPat get get
         1 -> liftM AssertionPat get
         2 -> return MakeMeta
         _ -> undefined


-- | 'PatternF' is the type of pattern shaped containers for terms. The
-- bifunctoriality and bifoldability of 'PatternFF' gives rise to the
-- functoriality and foldability of 'PatternF', meaning we can use it as a
-- sub-sort of term shape. This ought to be generic, but Haskell can'

newtype PatternF a = PatternF { unwrapPatternF :: Scope (PatternFF a) }


instance Eq1 PatternF where
  eq1 (PatternF x) (PatternF x') = x == x'


instance Ord1 PatternF where
  compare1 (PatternF x) (PatternF x') = compare x x'


type Pattern = ABT (PatternFF Term)


instance Functor PatternF where
  fmap f (PatternF x) = PatternF (translateScope (first f) x)


instance Foldable PatternF where
  foldMap f (PatternF x) = foldScope fmAlgVar (fmAlgRec f) fmAlgSc x
    where
      fmAlgVar :: Monoid m => Variable -> m
      fmAlgVar _ = mempty
      
      fmAlgRec :: Monoid m => (a -> m) -> PatternFF a m -> m
      fmAlgRec g = bifoldMap g id
      
      fmAlgSc :: Monoid m => Int -> m -> m
      fmAlgSc _ = id

instance Traversable PatternF where
  sequenceA (PatternF sc) = PatternF <$> bisequenceScopeF sc


instance Binary a => Binary (PatternF a) where
  put (PatternF sc) = put sc
  get = liftM PatternF get


-- | Because patterns need to have two domains of binders that essentially
-- bind the same variables — the binders in the assertions, and the binders
-- in the scopes themselves — we benefit from being able to bind both at once.
-- The 'patternScope' function does precisely this, by first binding the
-- assertion scopes, and then by binding the pattern scope.

patternScope :: [String] -> Pattern -> PatternF (Scope TermF)
patternScope xs = pattern . assertions
  where
    assertions :: Pattern -> ABT (PatternFF (Scope TermF))
    assertions = translate (first (scope xs))
    
    pattern :: ABT (PatternFF (Scope TermF)) -> PatternF (Scope TermF)
    pattern = PatternF . scope xs


-- | We can simultaneously instantiate the pattern scope and the inner
-- asserted term scopes.

patternInstantiate :: PatternF (Scope TermF)
                   -> [ABT (PatternFF Term)]
                   -> [Term]
                   -> ABT (PatternFF Term)
patternInstantiate p ps xs = pattern (assertions p)
  where
    assertions :: PatternF (Scope TermF) -> PatternF Term
    assertions = fmap (\sc -> instantiate sc xs)
    
    pattern :: PatternF Term -> ABT (PatternFF Term)
    pattern (PatternF x) = instantiate x ps


-- | It's also useful to be able to strip off binders to get a sort of body.

patternBody :: PatternF (Scope TermF) -> Pattern
patternBody (PatternF sc) = translate (first body) (body sc)


annH :: Term -> Term -> Term
annH m t = In (Ann (scope [] m) (scope [] t))

funH :: Plicity -> String -> Term -> Term -> Term
funH plic x a b = In (Fun plic (scope [] a) (scope [x] b))

lamH :: Plicity -> String -> Term -> Term
lamH plic x b = In (Lam plic (scope [x] b))

appH :: Plicity -> Term -> Term -> Term
appH plic f x = In (App plic (scope [] f) (scope [] x))

conH :: Name -> [(Plicity,Term)] -> Term
conH c as = In (Con c [ (plic, scope [] a) | (plic,a) <- as ])

caseH :: [Term] -> CaseMotive -> [Clause] -> Term
caseH as mot cls = In (Case (map (scope []) as) mot cls)

caseMotiveH :: [String] -> [Term] -> Term -> CaseMotive
caseMotiveH xs as b = CaseMotive (bindingTelescopeH xs as b)

clauseH :: [String] -> [Pattern] -> Term -> Clause
clauseH xs ps b = Clause (map (patternScope xs) ps) (scope xs b)

conPatH :: Name -> [(Plicity,Pattern)] -> Pattern
conPatH c ps = In (ConPat c [ (plic, scope [] p) | (plic,p) <- ps ])

assertionPatH :: Term -> Pattern
assertionPatH m = In (AssertionPat m)

recordTypeH :: [(String,Term)] -> Term
recordTypeH fas = In (RecordType fs (telescopeH fs as))
  where
    (fs,as) = unzip fas

recordConH :: [(String,Term)] -> Term
recordConH fields = In (RecordCon [ (f, scope [] a) | (f,a) <- fields ])

recordProjH :: Term -> String -> Term
recordProjH r x = In (RecordProj (scope [] r) x)

quotedTypeH :: [String] -> Term -> Term
quotedTypeH resets a = In (QuotedType resets (scope [] a))

quoteH :: Term -> Term
quoteH m = In (Quote (scope [] m))

unquoteH :: Term -> Term
unquoteH m = In (Unquote (scope [] m))

continueH :: Term -> Term
continueH m = In (Continue (scope [] m))

shiftH :: String -> Term -> Term
shiftH res m = In (Shift res (scope [] m))

resetH :: String -> Term -> Term
resetH res m = In (Reset res (scope [] m))

requireH :: String -> Term -> Term -> Term
requireH x a m = In (Require (scope [] a) (scope [x] m))

externalH :: Int -> Term -> Term
externalH i a = In (External i (scope [] a))

postulateH :: Term -> Term
postulateH a = In (Postulate (scope [] a))

mkStrH :: String -> Term
mkStrH s = In (MkStr s)





-- | Terms have a variety of positions in which they can have subterms. Due
-- to plicities, both 'AppArg' and 'ConArg' have plicity arguments as well.
-- 'FunArg' doesn't need one because the notation always wraps with either
-- parens or braces, independent of what the argument type is.

data TermParenLoc
  = AnnTerm | AnnType
  | FunArg | FunRet
  | LamBody | AppFun | AppArg Plicity
  | ConArg Plicity | CaseArg | MotiveArg | MotiveRet | ClauseBody
  | AssertionPatArg
  | RecFieldType | RecFieldVal | RecProjArg
  | QuotedTypeArg | QuoteArg | UnquoteArg
  | ContinueArg | ShiftArg | ResetArg
  | RequireType | RequireBody
  deriving (Eq)


instance Parens Term where
  type Loc Term = TermParenLoc
  
  parenLoc (Var _) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Defined _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Ann _ _)) =
    [FunArg,FunRet,LamBody,AppArg Impl,ConArg Impl,CaseArg,MotiveRet
    ,ClauseBody,RecFieldVal,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In Str) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (MkStr _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In Type) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Fun _ _ _)) =
    [AnnType,FunArg,FunRet,LamBody,AppArg Impl,ConArg Impl,CaseArg,MotiveArg
    ,MotiveRet,ClauseBody,RecFieldType,RecFieldVal,ShiftArg,ResetArg
    ,RequireType,RequireBody
    ]
  parenLoc (In (Lam _ _)) =
    [AnnType,FunArg,FunRet,LamBody,AppArg Impl,ConArg Impl,CaseArg,MotiveArg
    ,MotiveRet,ClauseBody,RecFieldType,RecFieldVal,ShiftArg,ResetArg
    ,RequireType,RequireBody
    ]
  parenLoc (In (App _ _ _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Impl,ConArg Impl
    ,CaseArg,MotiveArg,MotiveRet,ClauseBody,RecFieldType,RecFieldVal
    ,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Con _ [])) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Con _ _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Impl,ConArg Impl
    ,CaseArg,MotiveArg,MotiveRet,ClauseBody,RecFieldType,RecFieldVal
    ,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Case _ _ _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (RecordType _ _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (RecordCon _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (RecordProj _ _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (QuotedType _ _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Impl,ConArg Impl
    ,CaseArg,MotiveArg,MotiveRet,ClauseBody,RecFieldType,RecFieldVal
    ,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Quote _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg,ContinueArg,ShiftArg
    ,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Unquote _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg,ContinueArg,ShiftArg
    ,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Continue _)) =
    [AnnType,FunArg,FunRet,LamBody,AppArg Impl,ConArg Impl,CaseArg,MotiveArg
    ,MotiveRet,ClauseBody,RecFieldType,RecFieldVal,ShiftArg,ResetArg
    ,RequireType,RequireBody
    ]
  parenLoc (In (Shift _ _)) =
    [AnnType,FunArg,FunRet,LamBody,AppArg Impl,ConArg Impl,CaseArg,MotiveArg
    ,MotiveRet,ClauseBody,RecFieldType,RecFieldVal,ShiftArg,ResetArg
    ,RequireType,RequireBody
    ]
  parenLoc (In (Reset _ _)) =
    [AnnType,FunArg,FunRet,LamBody,AppArg Impl,ConArg Impl,CaseArg,MotiveArg
    ,MotiveRet,ClauseBody,RecFieldType,RecFieldVal,ShiftArg,ResetArg
    ,RequireType,RequireBody
    ]
  parenLoc (In (Require _ _)) =
    [AnnType,FunArg,FunRet,LamBody,AppArg Impl,ConArg Impl,CaseArg,MotiveArg
    ,MotiveRet,ClauseBody,RecFieldType,RecFieldVal,ShiftArg,ResetArg
    ,RequireType,RequireBody
    ]
  parenLoc (In (External _ _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  parenLoc (In (Postulate _)) =
    [AnnTerm,AnnType,FunArg,FunRet,LamBody,AppFun,AppArg Expl,AppArg Impl
    ,ConArg Expl,ConArg Impl,CaseArg,MotiveArg,MotiveRet,ClauseBody
    ,AssertionPatArg,RecFieldType,RecFieldVal,RecProjArg,QuotedTypeArg
    ,QuoteArg,UnquoteArg,ContinueArg,ShiftArg,ResetArg,RequireType,RequireBody
    ]
  
  parenRec (Var v) =
    name v
  parenRec (In (Defined n)) = showName n
  parenRec (In (Ann m ty)) =
    parenthesize (Just AnnTerm) (instantiate0 m)
      ++ " : " ++ parenthesize (Just AnnType) (instantiate0 ty)
  parenRec (In Str) = "String"
  parenRec (In (MkStr s)) = show s
  parenRec (In Type) = "Type"
  parenRec (In (Fun plic a sc)) =
    wrap
      plic
      (unwords (names sc) 
         ++ " : " ++ parenthesize (Just FunArg) (instantiate0 a))
      ++ " -> " ++ parenthesize (Just FunRet) (body sc)
    where
      wrap Expl x = "(" ++ x ++ ")"
      wrap Impl x = "{" ++ x ++ "}"
  parenRec (In (Lam plic sc)) =
    "\\" ++ wrap plic (unwords (names sc))
      ++ " -> " ++ parenthesize (Just LamBody)
                     (body sc)
    where
      wrap Expl x = x
      wrap Impl x = "{" ++ x ++ "}"
  parenRec (In (App plic f a)) =
    parenthesize (Just AppFun) (instantiate0 f)
      ++ " " ++ wrap plic
                     (parenthesize
                       (Just (AppArg plic))
                       (instantiate0 a))
    where
      wrap Expl x = x
      wrap Impl x = "{" ++ x ++ "}"
  parenRec (In (Con c [])) = showName c
  parenRec (In (Con c as)) =
    showName c ++ " "
      ++ unwords [ wrap plic
                        (parenthesize
                          (Just (ConArg plic))
                          (instantiate0 a))
                 | (plic,a) <- as
                 ]
    where
      wrap Expl x = x
      wrap Impl x = "{" ++ x ++ "}"
  parenRec (In (Case ms mot cs)) =
    "case "
      ++ intercalate
           " || "
           (map (parenthesize (Just CaseArg).instantiate0) ms)
      ++ " motive " ++ pretty mot
      ++ " of " ++ intercalate " | " (map auxClause cs) ++ " end"
    where
      auxClause (Clause pscs sc)
        = intercalate " || " (map (pretty.body.unwrapPatternF.fmap body) pscs)
            ++ " -> " ++ parenthesize (Just ClauseBody) (body sc)
  parenRec (In (RecordType fields (Telescope ascs))) =
    case types of
      [] -> "Rec {}"
      fs -> "Rec { " ++ intercalate ", " fs ++ " }"
    where
      types =
        zipWith
          (\field asc ->
            field ++ " : " ++ parenthesize (Just RecFieldType) (body asc))
          fields
          ascs
  parenRec (In (RecordCon fields)) =
    if null fields
       then "{}"
       else "{ "
            ++ intercalate
                 ", "
                 [ x ++ " = "
                     ++ parenthesize
                          (Just RecFieldVal)
                          (instantiate0 t)
                 | (x,t) <- fields
                 ]
            ++ " }"
  parenRec (In (RecordProj r x)) =
    parenthesize
      (Just RecProjArg)
      (instantiate0 r)
    ++ "." ++ x
  parenRec (In (QuotedType resets a)) =
    "Quoted[" ++ intercalate "," resets ++ "] "
      ++ parenthesize (Just QuotedTypeArg) (instantiate0 a)
  parenRec (In (Quote m)) =
    "`" ++ parenthesize (Just QuoteArg) (instantiate0 m)
  parenRec (In (Unquote m)) =
    "~" ++ parenthesize (Just UnquoteArg) (instantiate0 m)
  parenRec (In (Continue m)) =
    "continue " ++ parenthesize (Just ContinueArg) (instantiate0 m)
  parenRec (In (Shift res m)) =
    "shift " ++ res ++ " in "
      ++ parenthesize (Just ShiftArg) (instantiate0 m)
  parenRec (In (Reset res m)) =
    "reset " ++ res ++ " in "
      ++ parenthesize (Just ResetArg) (instantiate0 m)
  parenRec (In (Require a sc)) =
    "require " ++ head (names sc) ++ " : "
      ++ parenthesize (Just RequireType) (instantiate0 a)
      ++ " in " ++ parenthesize (Just RequireBody) (body sc)
  parenRec (In (External i a)) =
    "<external " ++ show i ++ " : "
      ++ parenthesize Nothing (instantiate0 a) ++ " >"
  parenRec (In (Postulate a)) =
    "<postulate " ++ parenthesize Nothing (instantiate0 a) ++ " >"





data CaseMotiveParenLoc


instance Eq CaseMotiveParenLoc where
  _ == _ = True


instance Parens CaseMotive where
  type Loc CaseMotive = CaseMotiveParenLoc
  
  parenLoc _ = []
  
  parenRec (CaseMotive (BindingTelescope ascs bsc)) =
    binders ++ " || " ++ pretty (body bsc)
    where
      binders =
        intercalate " || "
          (zipWith
             (\n a -> "(" ++ n ++ " : " ++ a ++ ")")
             ns
             as)
      as = map (pretty.body) ascs
      ns = names bsc





data PatternParenLoc
  = ConPatArg Plicity
  deriving (Eq)


instance Parens Pattern where
  type Loc Pattern = PatternParenLoc
  
  parenLoc (Var _)               = [ConPatArg Expl, ConPatArg Impl]
  parenLoc (In (ConPat _ []))    = [ConPatArg Expl, ConPatArg Impl]
  parenLoc (In (ConPat _ _))     = [ConPatArg Impl]
  parenLoc (In (AssertionPat _)) = [ConPatArg Expl, ConPatArg Impl]
  parenLoc (In MakeMeta)         = [ConPatArg Expl, ConPatArg Impl]
  
  parenRec (Var v) =
    name v
  parenRec (In (ConPat c [])) = showName c
  parenRec (In (ConPat c ps)) =
    showName c ++ " "
      ++ unwords [ wrap plic
                        (parenthesize
                          (Just (ConPatArg plic))
                          (instantiate0 p))
                 | (plic,p) <- ps
                 ]
    where
      wrap Expl x = x
      wrap Impl x = "{" ++ x ++ "}"
  parenRec (In (AssertionPat m)) =
    "." ++ parenthesize (Just AssertionPatArg) m
  parenRec (In MakeMeta) = "?makemeta"