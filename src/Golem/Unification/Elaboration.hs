{-# OPTIONS -Wall #-}







-- | This module defines how elaboration of programs is performed.

module Golem.Unification.Elaboration where

import Golem.Utils.ABT
import Golem.Utils.Elaborator
import Golem.Utils.Names
import Golem.Utils.Plicity
import Golem.Utils.Pretty
import Golem.Utils.Telescope
import Golem.Utils.Vars

import Golem.Core.ConSig
import Golem.Core.DeclArg
import Golem.Core.Program
import Golem.Core.Term
import Golem.Unification.Elaborator
import Golem.Unification.TypeChecking

import Control.Monad
import Control.Monad.Except
import Data.List (inits,nub,partition)
import Data.Maybe (fromJust)







-- | We can add a new defined value declaration given a name, term, and type.

addDeclaration :: (String,String) -> Term -> Term -> Elaborator ()
addDeclaration n def ty = addElab definitions [(n,(def,ty))]


-- | We can add a new constructor by giving a name and a signature.

addConstructor :: (String,String) -> ConSig -> Elaborator ()
addConstructor c consig = addElab signature [(c,consig)]


-- | We can add a constructor to a module other than the current one.

addConstructorToModule :: String -> String -> ConSig -> Elaborator ()
addConstructorToModule m c consig
  = do sig <- getElab signature
       putElab signature (((m,c),consig):sig)


-- | We can add an alias to the current collection of aliases.

addAlias :: String -> Elaborator ()
addAlias n =
  do als <- getElab aliases
     m <- getElab moduleName
     putElab aliases ((Left n,(m,n)):als)


-- | We can add an alias for something other than a name in the current
-- module.

addAliasFor :: Either String (String,String)
            -> (String,String)
            -> Elaborator ()
addAliasFor a b =
  do als <- getElab aliases
     putElab aliases ((a,b):als)


-- | We can add a module name to the current collection.

addModuleName :: String -> Elaborator ()
addModuleName m
  = do ms <- getElab moduleNames
       unless (not (m `elem` ms))
         $ throwError $ "A module is already declared with the name " ++ m
       putElab moduleNames (m:ms)

-- | We can add a reset point declaration to the current collection.

addResetPoint :: String -> Term -> Term -> Elaborator ()
addResetPoint res lower upper =
  do resets <- getElab resetPoints
     case lookup res resets of
       Nothing -> putElab resetPoints ((res,(lower,upper)):resets)
       Just _ ->
         throwError $ "The reset point " ++ res ++ " is already declared."





-- | Elaborating a term declaration takes one of two forms, depending on what
-- kind of declaration is being elaborated. A definition of the form
-- @let f : A = M end@ is elaborated directly, while a definition of the form
-- @let f : A where f x y z = M end@ is first transformed into the former
-- type of declaration, and then elaborated.
--
-- This corresponds to the elaboration judgment @Σ ⊢ let x : A = M end def⇝ Δ@
-- which is defined as
--
-- @
--    Δ # x   ⊢ A ▹ A' ⇐ Type true   x : A' true ⊢ M ▹ M'  ⇐ A' true
--    --------------------------------------------------------------
--          Δ ⊢ let x : A = M end def⇝ Δ, MOD.x = M' : A' true
--
--    Δo # x
--    ⊢ (x0 : A0) -> ... -> (xn : An) -> B ▹ T ⇐ Type true
--    ⊢ (x0 : A0) || ... || (xn : An) || B ▹ Mot motive
--    -------------------------------------------------------------
--    Δo ⊢ let family f (x0 : A0) ... (xn : An) : B end
--           def⇝ Δo, MOD.f = [] : T with Mot
--
--    Δo ∋ MOD.f = _ : T with Cls at Mot
--    ⊢ \x0 ... xn -> case x0 || ... || xn motive Mot of Cls | Cls' end
--        ▹ M ⇐ T true
---   -----------------------------------------------------------------
--    Δo ⊢ let instance f Cls' end
--           def⇝ Δo{Mod.f = M : T with Cls,Cls' at Mot}
-- @
--
-- where @Δ # x@ means that @x@ is not defined in @Δ@.

elabTermDecl :: TermDeclaration -> Elaborator ()
elabTermDecl d@(TermDeclaration n ty0 def0) =
  do let ty = freeToDefined (In . Defined . BareLocal) ty0
         def = freeToDefined (In . Defined . BareLocal) def0
     m <- getElab moduleName
     when' (typeInDefinitions (BareLocal n))
         $ throwError ("Term already defined: " ++ n)
     addAlias n
     elty <-
       catchError
         (check ty (NormalTerm (In Type)))
         (\e ->
           throwError $
             "Error when checking the type in declaration:\n\n" ++ show d
             ++ "\n\nNamely:\n" ++ e)
     ety <-
       catchError
         (evaluate (SubstitutedTerm elty)) -- @ty@ has no metas to substitute.
         (\e ->
           throwError $
             "Error when evaluating the type in declaration:\n\n" ++ show d
             ++ "\n\nNamely:\n" ++ e)
     eldef <-
       catchError
         (extendElab
            definitions
            [((m,n),(def,normTerm ety))]
            (check def ety))
         (\e ->
           throwError $
             "Error when checking the type of the value in the"
             ++ " declaration:\n\n" ++ show d ++ "\n\nNamely:\n" ++ e)
     addDeclaration (m,n) eldef (normTerm ety)
elabTermDecl d@(WhereDeclaration n ty preclauses) =
  case preclauses of
    [] ->
      throwError $
        "Cannot create an empty let-where definition: " ++ show d
    [(plics,(xs,ps,b))] | all isVarPat ps ->
      elabTermDecl
        (TermDeclaration
           n
           ty
           (helperFold (uncurry lamH) (zip plics xs) b))
    (_,(_,ps0,_)):_ ->
      do let lps0 = length ps0
         unless (all (\(_,(_,ps,_)) -> length ps == lps0) preclauses)
           $ throwError
             $ "Mismatching number of patterns in different "
            ++ "clauses of a pattern matching function in declaration: "
            ++ show d
         let (plics:plicss) = map fst preclauses
         unless (all (plics==) plicss)
           $ throwError
           $ "Mismatching plicities in different clauses of a "
          ++ "pattern matching function in declaration: "
          ++ show d
         case truePlicities plics ty of
           Nothing ->
             throwError $ "Cannot build a case expression motive from the "
                       ++ "type " ++ pretty ty ++ " in declaration: "
                       ++ show d
           Just truePlics ->
             do let mot = motiveAux (length truePlics) ty
                    clauses = [ clauseH xs (truePatterns truePlics ps) b
                              | (_,(xs,ps,b)) <- preclauses
                              ]
                    xs0 = [ "x" ++ show i | i <- [0..length ps0-1] ]
                    plicsForLambdas = map (either id id) truePlics
                elabTermDecl
                  (TermDeclaration
                     n
                     ty
                     (helperFold
                        (uncurry lamH)
                        (zip plicsForLambdas xs0)
                        (caseH (map (Var . Free . FreeVar) xs0) mot clauses)))
  where
    isVarPat :: Pattern -> Bool
    isVarPat (Var _) = True
    isVarPat _ = False
    
    truePlicities :: [Plicity] -> Term -> Maybe [Either Plicity Plicity]
    truePlicities [] _ = Just []
    truePlicities (Expl:plics) (In (Fun Expl _ sc)) =
      do rest <- truePlicities plics (body sc)
         return $ Right Expl : rest
    truePlicities (Impl:plics) (In (Fun Impl _ sc)) =
      do rest <- truePlicities plics (body sc)
         return $ Right Impl : rest
    truePlicities (Expl:plics) (In (Fun Impl _ sc)) =
      do rest <- truePlicities (Expl : plics) (body sc)
         return $ Left Impl : rest
    truePlicities (Impl:_) (In (Fun Expl _ _)) = Nothing
    truePlicities _ _ =
      error "This case of 'truePlicities' should never have been reached."
    
    motiveAux :: Int -> Term -> CaseMotive
    motiveAux i0 t0 =
      let (ns,asb) = go i0 t0
          scs = zipWith scope (inits ns) asb
          ascs = init scs
          bsc = last scs
      in CaseMotive (BindingTelescope ascs bsc)
      where
        go :: Int -> Term -> ([String],[Term])
        go 0 b = ([],[b])
        go i (In (Fun _ a sc)) =
          let ([x],t) = descope sc
              (xs,as) = go (i-1) t
          in (x:xs, instantiate0 a:as)
        go _ _ =
          error $ "This case of 'go' in 'motiveAux' should never have been"
               ++ " reached."
    
    truePatterns :: [Either Plicity Plicity] -> [Pattern] -> [Pattern]
    truePatterns [] [] = []
    truePatterns (Right _:plics) (p:ps) =
      p : truePatterns plics ps
    truePatterns (Left _:plics) ps =
      In MakeMeta : truePatterns plics ps
    truePatterns _ _ =
      error "This case of 'truePatterns' should never have been reached."
elabTermDecl d@(LetFamilyDeclaration n args ty0) =
  do m <- getElab moduleName
     when' (typeInDefinitions (BareLocal n))
       $ throwError 
         $ "A term named " ++ n
        ++ " is already defined when declaring family "
        ++ show d
     let plics = [ plic | DeclArg plic _ _ <- args ]
         ty = freeToDefined (In . Defined . BareLocal)
                $ helperFold (\(DeclArg plic x t) -> funH plic x t) args ty0
         (xs,ts) = unzip [ (x,t) | DeclArg _ x t <- args ]
         mot = fmap (freeToDefinedScope (In . Defined . BareLocal))
                    (caseMotiveH xs ts ty0)
     elty <-
       catchError
         (check ty (NormalTerm (In Type)))
         (\e ->
           throwError $
             "Error when checking the type in declaration:\n\n" ++ show d
             ++ "\n\nNamely:\n" ++ e)
     mot' <-
       catchError
         (checkifyCaseMotive mot)
         (\e ->
           throwError $
             "Error when checking the motive in declaration:\n\n" ++ show d
             ++ "\n\nNamely:\n" ++ e)
     fs <- getElab openFunctions
     case lookup (m,n) fs of
       Nothing ->
         do putElab openFunctions (((m,n),(elty,plics,mot',[])) : fs)
            let xs0 = [ "x" ++ show i | i <- [0..length plics-1] ]
                initialDef =
                  helperFold
                    (uncurry lamH)
                    (zip plics xs0)
                    (caseH (map (Var . Free . FreeVar) xs0) mot' [])
            addAlias n
            ety <-
              catchError
                (evaluate (SubstitutedTerm elty))
                (\e ->
                  throwError $
                    "Error when evaluating the type in declaration:\n\n"
                    ++ show d ++ "\n\nNamely:\n" ++ e)
            initialDef' <-
              catchError
                (extendElab
                  definitions
                  [((m,n),(initialDef,elty))]
                  (check initialDef ety))
                (\e ->
                  throwError $
                    "Error when checking the type of the value in the"
                    ++ " declaration:\n\n" ++ show d ++ "\n\nNamely:\n" ++ e)
            addDeclaration (m,n) initialDef' elty
       Just _ ->
         throwError
           $ "The open function " ++ show (Absolute m n)
          ++ " has already been declared when declaring family "
          ++ show d
elabTermDecl d@(LetInstanceDeclaration n preclauses) =
  do (m',n') <- unalias n
     fs <- getElab openFunctions
     case lookup (m',n') fs of
       Nothing ->
         throwError $ "No open function named " ++ show n
                   ++ " has been declared when declaring instance "
                   ++ show d
       Just (ty,plics,mot,clauses) ->
         do clauses'
              <- forM preclauses $ \(plics',(xs,ps,b)) -> do
                   case insertMetas plics plics' of
                     Nothing ->
                       throwError $ "Instance for open function "
                         ++ show n ++ " has invalid argument plicities"
                         ++ " in declaration " ++ show d
                     Just bs ->
                       return $
                         fmap (freeToDefinedScope
                                (In . Defined . BareLocal))
                                (clauseH xs (truePatterns bs ps) b)
            let newClauses = clauses ++ clauses'
                xs0 = [ "x" ++ show i | i <- [0..length plics-1] ]
                newDef =
                  helperFold
                    (uncurry lamH)
                    (zip plics xs0)
                    (caseH (map (Var . Free . FreeVar) xs0) mot newClauses)
                newOpenFunctions =
                  ((m',n'),(ty,plics,mot,newClauses))
                    : filter (\(p,_) -> p /= (m',n')) fs
            ety <-
              catchError
                (evaluate (SubstitutedTerm ty))
                (\e ->
                  throwError $
                   "Error when evaluating the type in declaration:\n\n"
                   ++ show d ++ "\n\nNamely:\n" ++ e)
            newDef' <-
              catchError
                (extendElab
                  definitions
                  [((m',n'), (newDef, ty))]
                  (check newDef ety))
                (\e ->
                  throwError $
                    "Error when checking the type of the value in the"
                    ++ " declaration:\n\n" ++ show d ++ "\n\nNamely:\n" ++ e)
            putElab openFunctions newOpenFunctions
            defs <- getElab definitions
            putElab definitions
              $ ((m',n'),(newDef',ty))
                  : filter (\(p,_) -> p /= (m',n')) defs
  where
    insertMetas :: [Plicity] -> [Plicity] -> Maybe [Bool]
    insertMetas [] [] = Just []
    insertMetas (Expl:args) (Expl:plics) =
      do rest <- insertMetas args plics
         return $ False:rest
    insertMetas (Expl:_) (Impl:_) = Nothing
    insertMetas (Impl:args) (Expl:plics) =
      do rest <- insertMetas args plics
         return $ True:rest
    insertMetas (Impl:args) (Impl:plics) =
      do rest <- insertMetas args plics
         return $ False:rest
    insertMetas _ _ = Nothing
    
    truePatterns :: [Bool] -> [Pattern] -> [Pattern]
    truePatterns [] [] = []
    truePatterns (False:plics) (p:ps)
      = p : truePatterns plics ps
    truePatterns (True:plics) ps
      = In MakeMeta : truePatterns plics ps
    truePatterns _ _ = undefined





-- | Elaboration of a constructor in this variant is a relatively simple
-- process. This corresponds to the elaboration judgment @Σ ⊢ c con⇝ Σ'@ which
-- is defined as
--
-- @
--       Σ # c   Γ ⊢ S ▹ S' consig
--    --------------------------------
--    Σ ⊢ alt[c](S) con⇝ Σ, MOD.c : S'
-- @
--
-- where @Σ # c@ means that @c@ is not a data constructor in @Σ@.

elabAlt :: String -> String -> ConSig -> Elaborator ()
elabAlt tycon c consig0
  = do let consig = freeToDefinedConSig consig0
       validConSig (BareLocal tycon) (BareLocal c) consig
       m <- getElab moduleName
       when' (typeInSignature (BareLocal c))
           $ throwError
             $ "Constructor " ++ c ++ " already declared when declaring "
            ++ " the type " ++ tycon
       addAlias c
       consig' <-
         catchError
           (checkifyConSig consig)
           (\e ->
             throwError $
               "Error when checking the signature for " ++ c ++ " in the"
               ++ " declaration for " ++ tycon ++ "\n\nNamely:\n" ++ e)
       addConstructor (m,c) consig'





-- | Elaboration of a constructor for a data instance is similar to normal
-- constructor elaboration.

elabInstanceAlt :: String
                -> Name
                -> String
                -> ConSig
                -> Elaborator ()
elabInstanceAlt m localTycon c consig0
  = do let consig = freeToDefinedConSig consig0
       validConSig localTycon (BareLocal c) consig
       when' (typeInSignature (BareLocal c))
           $ throwError
             $ "Constructor " ++ c ++ " already declared when declaring "
            ++ " the type instance " ++ showName localTycon
       sig <- getElab signature
       case lookup (m,c) sig of
         Just _
           -> throwError 
                $ "Constructor " ++ c ++ " already declared when declaring"
               ++ " a type instance for " ++ showName localTycon
         Nothing
           -> do addAliasFor (Left c) (m,c)
                 consig' <-
                   catchError
                     (checkifyConSig consig)
                     (\e ->
                       throwError $
                         "Error when checking the signature for " ++ c
                         ++ " in the declaration for " ++ showName localTycon
                         ++ "\n\nNamely:\n" ++ e)
                 addConstructorToModule m c consig'





-- | We've extracted the function to check whether or not a consig is valid
-- because of reuse.

validConSig :: Name -> Name -> ConSig -> Elaborator ()
validConSig tycon c (ConSig _ (BindingTelescope _ retsc)) =
  case body retsc of
    In (Con tc _) ->
      unless (tc == tycon)
        $ throwError $ "The constructor " ++ showName c ++ " should construct a"
                ++ " value of the type " ++ showName tycon ++ " but instead"
                ++ " produces a " ++ showName tc
    a ->
      throwError $ "The constructor " ++ showName c ++ " should constructor a"
                ++ " value of the type " ++ showName tycon ++ " but instead"
                ++ " produces " ++ pretty a





-- | Elaboration of a type constructor is similar to elaborating a data
-- constructor, except it includes elaborations for the constructors as well.
--
-- @
--    Σ # c
--    ⊢ (x0 : A0, ..., xn : An) Type ▹ S' consig
--    Σ, MOD.c : S' ⊢ L0 | ... | Ln cons⇝ Σ'
--    --------------------------------------------------------------------
--    Σ ⊢ data c (x0 : A0) ... (xn : An) where L0 | ... | L1 end tycon⇝ Σ'
-- @
--
-- where here @Σ # c@ means that @c@ is not a type constructor in @Σ@.

elabTypeDecl :: TypeDeclaration -> Elaborator ()
elabTypeDecl d@(TypeDeclaration tycon tyconargs alts)
  = do let tyconSig = freeToDefinedConSig (conSigH tyconargs (In Type))
       m <- getElab moduleName
       when' (typeInSignature (BareLocal tycon))
           $ throwError
             $ "Type constructor " ++ tycon ++ " already declared in: "
            ++ show d
       addAlias tycon
       tyconSig' <-
         catchError
           (checkifyConSig tyconSig)
           (\e ->
             throwError $
               "Error when checking the signature for the type constructor in"
               ++ " the declaration\n\n" ++ show d ++ "\n\nNamely:\n" ++ e)
       addConstructor (m,tycon) tyconSig'
       mapM_ (uncurry (elabAlt tycon)) alts
elabTypeDecl d@(DataFamilyDeclaration tycon tyconargs) =
  do let tyconSig = freeToDefinedConSig (conSigH tyconargs (In Type))
     when' (typeInSignature (BareLocal tycon))
         $ throwError ("Type constructor already declared: " ++ tycon)
     addAlias tycon
     tyconSig' <-
       catchError
         (checkifyConSig tyconSig)
         (\e ->
            throwError $
              "Error when checking the signature for the type constructor in"
              ++ " the declaration\n\n" ++ show d ++ "\n\nNamely:\n" ++ e)
     m <- getElab moduleName
     addConstructor (m,tycon) tyconSig'
     od <- getElab openData
     putElab openData ((m,tycon):od)
elabTypeDecl d@(DataInstanceDeclaration tycon alts) =
  do (m',c') <- unalias tycon
     od <- getElab openData
     unless ((m',c') `elem` od)
       $ throwError $
         "The constructor " ++ showName tycon ++ " is not an open data type "
         ++ " in the declaration " ++ show d
     mapM_ (uncurry (elabInstanceAlt m' tycon)) alts





-- | Elaborating a reset point corresponds to the judgment
-- @R ⊢ k resets A to B ⇝ R'@ which is defined as
--
-- @
--    R # k    ⊢ A ⇐ Type true   ⊢ B ⇐ Type true
--    -------------------------------------------
--    R ⊢ reset k from A to B ⇝ R, k : A resets B
-- @

elabResetDecl :: ResetDeclaration -> Elaborator ()
elabResetDecl (ResetDeclaration res lower upper) =
  do lower' <- check lower (NormalTerm (In Type))
     upper' <- check upper (NormalTerm (In Type))
     addResetPoint res lower' upper'





-- | We can require that a module exists to be imported.

ensureModuleExists :: String -> Elaborator ()
ensureModuleExists m
  = do ms <- getElab moduleNames
       unless (m `elem` ms)
         $ throwError $ "The module " ++ m ++ " is not a known module."


-- | We can ensure that the open-as settings are valid by checking that there
-- isn't already an open module with that name.

openAsIsValid :: Maybe String -> Elaborator ()
openAsIsValid Nothing = return ()
openAsIsValid (Just m')
  = do ms <- getElab moduleNames
       unless (not (m' `elem` ms))
         $ throwError $ "The module name " ++ m' ++ " is already in use."


-- | We can get the names of a module.

namesInModule :: String -> Elaborator [String]
namesInModule m =
  do defs <- getElab definitions
     sig <- getElab signature
     return $
       nub $ [ n | ((m',n),(_,_)) <- defs, m' == m ]
          ++ [ n | ((m',n),_) <- sig, m' == m ]


-- | We can ensure that the hiding-using settings are valid  by checking that
-- all of the relevant names exist in the module in question.

hidingUsingIsValid :: String -> Maybe HidingUsing -> Elaborator [String]
hidingUsingIsValid m Nothing =
  namesInModule m
hidingUsingIsValid m (Just (Hiding h)) =
  do known <- namesInModule m
     let missing = [ n | n <- h, not (n `elem` known) ]
     unless (null missing)
         $ throwError $ "The module " ++ m
                     ++ " does not declare these symbols: "
                     ++ unwords missing
     return [ n | n <- known, not (n `elem` h) ]
hidingUsingIsValid m (Just (Using u)) =
  do known <- namesInModule m
     let missing = [ n | n <- u, not (n `elem` known) ]
     unless (null missing)
         $ throwError $ "The module " ++ m
                     ++ " does not declare these symbols: "
                     ++ unwords missing
     return [ n | n <- known, n `elem` u ]


-- | We can make the full renaming map by adding to the renaming all of the
-- names that don't actually change.

fullRenamingMap
  :: [String] -> [(String,String)] -> Elaborator [(String,String)]
fullRenamingMap opened r =
  do let oldR = map fst r
     case [ n | n <- oldR, not (n `elem` opened) ] of
       [] ->
         return
           [ case lookup n r of
               Nothing -> (n,n)
               Just n' -> (n,n')
           | n <- opened
           ]
       missing ->
         throwError $
           "The follow names cannot be renamed because they have not been"
           ++ "opened: " ++ unwords missing


-- | We can check that the new names are valid by either checking that they're
-- being opened in a qualified module, or by checking that none of the new
-- names conflict with previously opened names.

newNamesAreValid
  :: String -> Maybe String -> [(String,String)] -> Elaborator ()
newNamesAreValid _ (Just _) _ =
  return ()
newNamesAreValid m Nothing r =
  do als <- getElab aliases
     let newR = map snd r
         aliased = map fst als
     case [ n | n <- newR, Left n `elem` aliased ] of
       [] -> return ()
       overlap ->
         throwError $
           "These symbols will be overlapping when the module "
             ++ m ++ " is opened: " ++ unwords overlap


-- | We can ensure that open settings are valid by ensuring the module to open
-- exists, that the open-as setting is valid, that the hiding-using settings
-- are valid, and finally that the renaming settings are valid.

ensureOpenSettingsAreValid :: [OpenSettings] -> Elaborator ()
ensureOpenSettingsAreValid oss
  = forM_ oss $ \(OpenSettings m a hu r) -> do
      ensureModuleExists m
      openAsIsValid a
      opened <- hidingUsingIsValid m hu
      fullRenaming <- fullRenamingMap opened r
      newNamesAreValid m a fullRenaming


-- | We can compute new aliases from open settings.

newAliases :: Signature -> Definitions -> [OpenSettings] -> Aliases
newAliases _ _ [] = []
newAliases sig defs (os:oss)
  = let als  = newAliasesFromSettings os
        als' = newAliases sig defs oss
    in als' ++ als
  where    
    newAliasesFromSettings :: OpenSettings -> Aliases
    newAliasesFromSettings (OpenSettings m a hu r)
      = let openedSymbols = [ (m',c) | ((m',c),_) <- sig, m' == m ]
                         ++ [ (m',x) | ((m',x),(_,_)) <- defs, m' == m ]
            usedSymbols = used hu openedSymbols
            renamedSymbols = renamed r usedSymbols
            asedSymbols = ased a renamedSymbols
        in asedSymbols
    
    used :: Maybe HidingUsing -> [(String,String)] -> [(String,String)]
    used Nothing            = id
    used (Just (Hiding ns)) = filter (\(_,n) -> not (n `elem` ns))
    used (Just (Using ns))  = filter (\(_,n) -> (n `elem` ns))
    
    renamed :: [(String,String)]
            -> [(String,String)]
            -> [(String,(String,String))]
    renamed r mns = [ (maybe n id (lookup n r), (m,n)) | (m,n) <- mns ]
    
    ased :: Maybe String
         -> [(String,(String,String))]
         -> [(Either String (String,String), (String,String))]
    ased Nothing   ns = [ (Left x, (m,n)) | (x,(m,n)) <- ns ]
    ased (Just m') ns = [ (Right (m',x), (m,n)) | (x,(m,n)) <- ns ]


-- | We can extend the current aliases with some open settings.

extendAliases :: [OpenSettings] -> Elaborator a -> Elaborator a
extendAliases settings tc
  = do ensureOpenSettingsAreValid settings
       als <- getElab aliases
       sig <- getElab signature
       defs <- getElab definitions
       let newAls = newAliases sig defs settings ++ als
       putElab aliases newAls
       x <- tc
       putElab aliases als
       return x





-- Elaborating a module involves chaining together the elaborations of each
-- kind of declaration. We can define it inductively as follows:
--
-- @
--    -----------------------
--    Σ ; Δ ⊢ e mod⇝ Σ' ; Δ'
--
--    Σ ⊢ data c where L0 | ... | L1 end tycon⇝ Σ'   Σ' ⊢ P mod⇝ Σ''
--    ---------------------------------------------------------------
--           Σ ⊢ data c where L0 | ... | L1 end ; P mod⇝ Σ''
--
--    Δ ⊢ let x : A = M end def⇝ Δ'   Δ' ⊢ P mod⇝ Δ''
--    ------------------------------------------------
--          Δ ⊢ let x : A = M end ; P mod⇝ Δ''
-- @

elabModule :: Module -> Elaborator ()
elabModule (Module m settings stmts0)
  = do addModuleName m
       putElab moduleName m
       catchError
         (ensureOpenSettingsAreValid settings)
         (\e ->
           throwError $
             "Open settings for the module " ++ m ++ " are not valid."
             ++ "\n\nNamely:\n" ++ e)
       als <- getElab aliases
       sig <- getElab signature
       defs <- getElab definitions
       let newAls = newAliases sig defs settings ++ als
       putElab aliases newAls
       catchError
         (go stmts0)
         (\e ->
           throwError $
             "Error in module " ++ m ++ ":\n\n" ++ e)
       putElab aliases als
       holesSolved
  where
    go :: [Statement] -> Elaborator ()
    go [] = return ()
    go (TyDecl td:stmts) = do elabTypeDecl td
                              go stmts
    go (TmDecl td:stmts) = do elabTermDecl td
                              go stmts
    go (ResetDecl r:stmts) = do elabResetDecl r
                                go stmts
    
    showContext :: Context -> String
    showContext [] = "ε"
    showContext ctx =
      unlines
        [ "  " ++ n ++ " : " ++ pretty t ++ " @ " ++ show lvl
        | (FreeVar n, QLJ t lvl) <- ctx
        ]
    
    holesSolved :: Elaborator ()
    holesSolved =
      do hctx <- getElab holeContext
         if null hctx
            then return ()
            else do
              ctxs <- getElab contextsForHoles
              let prettyHoles =
                    unlines
                      [ "?" ++ n ++ " : " ++ pretty t ++ " @ " ++ show lvl
                        ++ " in context\n"
                        ++ showContext (fromJust (lookup (FreeVar n) ctxs))
                      | (FreeVar n, QLJ t lvl) <- reverse hctx
                      ]
              throwError $
                "Not all holes in module " ++ m ++ " have been solved:\n\n"
                ++ prettyHoles





-- | We can sort modules by dependencies. This sort separates modules into
-- groups where each group has dependencies only on modules in previous
-- groups. We do this by splitting a set of remaining modules into two groups,
-- those that have dependencies only in previous modules, and those that
-- don't, and then we split on the ones that don't, over and over in this way,
-- until we've finished, or until the remaining modules have none that have
-- dependencies only in previous modules, which indicates circular
-- dependencies.

sortModules :: [Module] -> Elaborator [Module]
sortModules mods0 =
  do curMods <- getElab moduleNames
     go curMods mods0
  where
    splitModules :: [String] -> [Module] -> Elaborator ([Module], [Module])
    splitModules prev mods
      = let (next,rest) =
              partition (\(Module _ settings _) ->
                all (\s -> openModule s `elem` prev) settings) mods
        in if null next
           then diagnoseResolutionError mods
           else return (next,rest)
    
    diagnoseResolutionError :: [Module] -> Elaborator ([Module], [Module])
    diagnoseResolutionError rest =
      let (mnames, onamess) =
            unzip [ (mname, map openModule oss)
                  | Module mname oss _ <- rest
                  ]
          onames = nub (concat onamess)
          missing = filter (\x -> not (x `elem` mnames)) onames
      in if null missing
         then
           throwError $
             "The following modules have circular dependencies which " ++
             "prevent resolution: " ++ unwords [ n | Module n _ _ <- rest ]
         else do
           curMods <- getElab moduleNames
           throwError $
             "The following modules are opened but do not exist: " ++
             unwords missing ++ ". Current modules are: " ++
             unwords curMods
    
    go :: [String] -> [Module] -> Elaborator [Module]
    go _ []
      = return []
    go prev mods
      = do (next,rest) <- splitModules prev mods
           let newPrev = [ n | Module n _ _ <- next ]
           rest' <- go (newPrev ++ prev) rest
           return (next ++ rest')





-- | We can elaborate a whole program by sorting the modules it defines, and
-- then by elaborating them in that order. 

elabProgram :: Program -> Elaborator ()
elabProgram (Program mods)
  = do sortedMods <- sortModules mods
       mapM_ elabModule sortedMods