{-# OPTIONS -Wall #-}







-- | This module defines the system for processing linguistic input.

module APIUtils.InputProcessing where

import APIUtils.EntityBuilding
import APIUtils.GrammarExtraction
import APIUtils.WorldModel
import Charted.Charted
import Golem.Core.Decontinuization
import Golem.Core.Evaluation
import Golem.Core.RequireSolving
import Golem.Core.Term
import Golem.Utils.ABT
import Golem.Utils.Env
import Golem.Utils.Names







-- | 'ProcessingInfo' just bundles up the relevant pieces of input that are
-- needed for processing input.

data ProcessingInfo
  = ProcessingInfo
    { lexicon :: [LEWord]
    , grammarRules :: [LERule]
    , environment :: Env (String,String) Term
    , worldModel :: WorldModel
    }





-- | We can process input by parsing it, evaluating, peeling off the outermost
-- quote, decontinuizing, solving the requires, and then building the events.

processInput :: ProcessingInfo
             -> String
             -> Maybe (WorldModel, [EntityDescription])
processInput pinfo str =
  case solutions of
    [sem] -> makeTrue (worldModel pinfo) sem
    _     -> Nothing
  where
    lexr = convertWordsToLexer (lexicon pinfo)
    grammr = convertRulesToGrammar (grammarRules pinfo)
    discourseContext = worldModelToTerms (worldModel pinfo)
    solutions = do
      (sem,In (Con (Absolute "LE" "EXP") [])) <- parse grammr lexr str
      Just (In (Quote msc)) <- return (evalTerm (environment pinfo) sem)
      let m = instantiate0 msc
          decontm = decontinuize m
      solvedm <- solve (environment pinfo) discourseContext decontm
      Just finsem <- return (evalTerm (environment pinfo) solvedm)
      return finsem