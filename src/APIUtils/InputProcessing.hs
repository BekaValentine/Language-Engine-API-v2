{-# OPTIONS -Wall #-}
{-# LANGUAGE ViewPatterns #-}







-- | This module defines the system for processing linguistic input.

module APIUtils.InputProcessing where

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
import Golem.Utils.Pretty







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
             -> Either (Maybe (ParseError String String)) String
processInput pinfo str =
  case parse grammr lexr str of
    Left err ->
      Left (Just (prettyParseError pretty pretty err))
    Right parses ->
      case solutions parses of
        [sem] -> return (pretty sem)
        _     -> Left Nothing
  where
    lexr = convertWordsToLexer (lexicon pinfo)
    grammr = convertRulesToGrammar (grammarRules pinfo)
    discourseContext = worldModelToWitnessedTerms (worldModel pinfo)
    solutions parses = do
      (sem,In (Con (Absolute "LE" "QCDone")
                   [(_,instantiate0 -> In (Con (Absolute "LE" "EXPR") _))]))
        <- parses
      Just (In (Quote msc)) <- return (evalTermAtLevel (environment pinfo) sem 0)
      let m = instantiate0 msc
          decontm = decontinuize m
      Just normm <- return (evalTermAtLevel (environment pinfo) decontm 0)
      solvedm <- solve (environment pinfo) discourseContext normm
      Just finsem <- return (evalTerm (environment pinfo) solvedm)
      return finsem