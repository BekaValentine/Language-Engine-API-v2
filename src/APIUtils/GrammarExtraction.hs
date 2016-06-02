{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}







-- | This module defines tools for extracting words and rules from a set of
-- Language Engine declarations.

module APIUtils.GrammarExtraction where

import APIUtils.PackageBuilds
import Charted.Charted
import Golem.Utils.ABT
import Golem.Utils.Elaborator
import Golem.Utils.Env
import Golem.Utils.Names
import Golem.Utils.Plicity
import Golem.Utils.Pretty
import Golem.Utils.Unifier
import Golem.Utils.Vars
import Golem.Core.Parser
import Golem.Core.Term
import Golem.Unification.Elaborator
import Golem.Unification.Unification ()

import Control.Applicative
import Control.Monad.State
import Data.Binary
import Data.List
import GHC.Generics







-- | We define a type for representing words in a lexicon.

data LEWord
  = LEWord
    { form :: String
    , category :: Term
    , meaning :: Term
    }
  deriving (Generic)

instance Binary LEWord





-- | We can convert a term to an @LEWord@. We throw a Haskell error when the
-- term isn't a word b/c we should have filtered to use only actual words.

convertWord :: Term -> LEWord
convertWord (In (Con (Absolute "LE" "MkWord") [(_,sc)])) =
  case instantiate0 sc of
    In (RecordCon fields) ->
      case sortBy (\(x,_) (y,_) -> compare x y) fields of
        [("category",catsc),("form",formsc),("meaning",semsc)] ->
          case instantiate0 formsc of
            In (MkStr fm) ->
              LEWord fm (instantiate0 catsc) (instantiate0 semsc)
            _ -> convertWordError "A"
        _ -> convertWordError "B"
    _ -> convertWordError "C"
convertWord _ =
  convertWordError "D"


convertWordError :: String -> a
convertWordError loc =
  error $ "The 'convertWord' function has been given a non-word value at "
            ++ loc





-- | We define a type for representing rulesin a grammar.

data LERule
  = LERule
    { ruleType :: Term
    , ruleMeaning :: Term
    }
  deriving (Generic)

instance Binary LERule





-- | We can convert a term to an @LERule@. We throw a Haskell error when the
-- term isn't a rule b/c we should have filtered to use only actual rules.

convertRule :: Term -> LERule
convertRule tm@(In (Con (Absolute "LE" "MkRule") [(_,sc)])) =
  case instantiate0 sc of
    In (RecordCon fields) ->
      case sortBy (\(x,_) (y,_) -> compare x y) fields of
        [("ruleMeaning",semsc),("ruleType",tysc)] ->
          LERule (instantiate0 tysc) (instantiate0 semsc)
        _ -> convertRuleError ("A: " ++ pretty tm)
    _ -> convertRuleError ("B: " ++ pretty tm)
convertRule tm =
  convertRuleError ("C: " ++ pretty tm)


convertRuleError :: String -> a
convertRuleError loc =
  error $ "The 'convertRule' function has been given a non-rule value at "
            ++ loc





-- | We can test if a definition is a word by checking its type.

isWordType :: Term -> Bool
isWordType (In (Con (Absolute "LE" "Word") _)) = True
isWordType _ = False





-- | We can test if a definition is a rule by checking its type.

isRuleType :: Term -> Bool
isRuleType (In (Con (Absolute "LE" "Rule") _)) = True
isRuleType _ = False





-- | We can filter a set of definitions to have only those that define either
-- words or rules, separated out as such.

filterWordsAndRules :: Definitions -> ([LEWord],[LERule])
filterWordsAndRules = foldl f ([],[])
  where
    f (wds,rls) (_,(m,t))
      | isWordType t = (convertWord m:wds, rls)
      | isRuleType t = (wds, convertRule m:rls)
      | otherwise    = (wds, rls)





-- | We can convert some words into a lexer. This lexer will turn string
-- literals of the form "foo" and 'foo' into words of category STRING

convertWordsToLexer :: [LEWord] -> Lexer Term Term
convertWordsToLexer wds fm0
  | isString fm0 =
    -- if fm0 is "foo" then it means `"foo" and has cat LE.STRING
    [(quoteH (mkStrH fm0), In (Con (Absolute "LE" "STRING") []))]
  | otherwise =
    do LEWord fm cat sem <- wds
       guard (fm == fm0)
       return (sem,cat)
  where
    isString ('\"':_) = True
    isString _        = False





-- | We can convert a single rule to a Charted rule.

convertLERuleToChartedRule :: LERule -> Rule Term Term
convertLERuleToChartedRule (LERule rty rsem) =
  goQRule
    (ElabState [] [] [] [] (MetaVar 0) [] "" [] [] [] 0 [] [] [])
    []
    rty
  where
    goQRule :: ElabState -> [Term] -> Term -> Rule Term Term
    goQRule elabState impl (In (Con (Absolute "LE" "QRForall") [_,(_,sc)])) =
      let Right (m,elabState') =
            runStateT (nextElab nextMeta) elabState
          x = Var (Meta m)
      in goQRule elabState' (impl ++ [x]) (instantiate sc [x])
    goQRule elabState impl (In (Con (Absolute "LE" "QRDone") [(_,sc)])) =
      goRule elabState impl [] (instantiate0 sc)
    goQRule _ _ _ =
      error "The function goQRule was given a non-QRule term."
    
    goRule :: ElabState -> [Term] -> [Term] -> Term -> Rule Term Term
    goRule elabState impl expl (In (Con (Absolute "LE" "RDone") [(_,sc)])) =
      if _nextMeta elabState == MetaVar (length (_substitution elabState))
      then let returnCat = substMetas (_substitution elabState) (instantiate0 sc)
               appliedToImpl = foldl' (appH Impl) rsem impl
               appliedToExpl = foldl' (appH Expl) appliedToImpl expl
               returnTerm = substMetas (_substitution elabState) appliedToExpl
           in return (returnTerm, returnCat)
      else empty
    goRule elabState
           impl
           expl
           (In (Con (Absolute "LE" "RArg") [(_,asc),(_,bsc)]))
           =
      do cat <- nextLabel
         let argCat = substMetas (_substitution elabState) (instantiate0 asc)
             uniRes =
               runStateT
                 (unifyJ substitution context cat argCat)
                 elabState
         case uniRes of
           Left _ -> empty
           Right (_,elabState') ->
             do val <- nextData
                goRule
                  elabState'
                  impl
                  (expl ++ [val])
                  (substMetas (_substitution elabState')
                              (instantiate0 bsc))
    goRule _ _ _ _ =
      error "The function goRule was given a non-Rule term."





-- | We can convert a bunch of rules to a grammar.

convertRulesToGrammar :: [LERule] -> Grammar Term Term
convertRulesToGrammar = map convertLERuleToChartedRule





-- | An 'LEExtract' is just a triple of an environment, words, and rules.

data LEExtract
  = LEExtract
    { extractEnvironment :: Env (String,String) Term
    , extractWords :: [LEWord]
    , extractRules :: [LERule]
    }
  deriving (Generic)

instance Binary LEExtract

instance Show LEExtract where
  show (LEExtract env wds rles) = show (length env) ++ "," ++ show (length wds) ++ "," ++ show (length rles)





-- | An empty extract is just an extract with no data in the lists.

emptyExtract :: LEExtract
emptyExtract = LEExtract [] [] []





-- | Multiple extracts can be combined.

combineExtracts :: [LEExtract] -> LEExtract
combineExtracts =
  foldl' (\(LEExtract env wds rles) (LEExtract env' wds' rles') ->
            LEExtract (env ++ env') (wds ++ wds') (rles ++ rles'))
         (LEExtract [] [] [])


-- | The full extraction for a program involves parsing, elaborating, then
-- extracting the words and rules.

extract :: [PackageBuild]
        -> String
        -> Either String LEExtract
extract bg src =
  do prog <- parseProgram src
     pkg <- buildWithPreludes bg prog
     let (wds,rles) = filterWordsAndRules (packageDefinitions pkg)
     return LEExtract
            { extractEnvironment =
                definitionsToEnvironment (packageDefinitions pkg)
            , extractWords = wds
            , extractRules = rles
            }