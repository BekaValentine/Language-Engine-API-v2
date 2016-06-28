{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golem.Core.Parser where

import Control.Monad.Reader
import Data.List (foldl')
import Text.Parsec
import qualified Text.Parsec.Token as Token

import Golem.Utils.ABT hiding (shift)
import Golem.Utils.Names
import Golem.Utils.Plicity
import Golem.Utils.Vars

import Golem.Core.ConSig
import Golem.Core.DeclArg
import Golem.Core.Term
import Golem.Core.Program







infixl 9 >>=?

(>>=?) :: Parsec String u a -> (a -> Parsec String u a) -> Parsec String u a
p >>=? f =
  do x <- p
     option x (f x)


(<|>?) :: (a -> Parsec String u a)
       -> (a -> Parsec String u a)
       -> (a -> Parsec String u a)
p <|>? q = \x -> p x <|> q x
     
     
     

-- Language Definition

languageDef :: Token.LanguageDef st
languageDef = Token.LanguageDef
              { Token.commentStart = "{-"
              , Token.commentEnd = "-}"
              , Token.commentLine = "--"
              , Token.nestedComments = True
              , Token.identStart = letter <|> char '_'
              , Token.identLetter = alphaNum <|> char '_' <|> char '\''
              , Token.opStart = oneOf ""
              , Token.opLetter = oneOf ""
              , Token.reservedNames =
                  ["data","case","motive","of","end"
                  ,"where","let","Type","module","open"
                  ,"opening","as","using","hiding"
                  ,"renaming","to","Rec","family","instance"
                  ,"Quoted","continue","shift","reset"
                  ,"from","in","require","String"
                  ,"forall"
                  ]
              , Token.reservedOpNames =
                  ["|","||","->","\\",":","::","=",",",".","`","~","\\\\","//",">","=>","*"]
              , Token.caseSensitive = True
              }

tokenParser :: Token.TokenParser st
tokenParser = Token.makeTokenParser languageDef

identifier :: Parsec String u String
identifier = Token.identifier tokenParser

reserved :: String -> Parsec String u ()
reserved = Token.reserved tokenParser

reservedOp :: String -> Parsec String u ()
reservedOp = Token.reservedOp tokenParser

parens :: Parsec String u a -> Parsec String u a
parens = Token.parens tokenParser

braces :: Parsec String u a -> Parsec String u a
braces = Token.braces tokenParser

symbol :: String -> Parsec String u String
symbol = Token.symbol tokenParser

stringLiteral :: Parsec String u String
stringLiteral = Token.stringLiteral tokenParser

whiteSpace :: Parsec String u ()
whiteSpace = Token.whiteSpace tokenParser





-- names

varName :: Parsec String u String
varName =
  do _ <- lookAhead (lower <|> char '_')
     identifier

decName :: Parsec String u String
decName =
  do _ <- lookAhead upper
     identifier







-- * Term Parsers


-- | The parser 'term' captures the grammar of terms as specified by
--
-- @
--   <term> ::= <binderFunType>
--            | <lambda>
--            | <require>
--            | <reset>
--            | <shift>
--            | <catMagic>
--            | <ruleMagic>
--            | (<conData> | <continue> | <quotedType>)
--                >>=? ( <annotationSuffix>
--                     | <noBinderFunTypeSuffix>
--                     )
--            | (<quote> | <unquote>)
--                >>=? <applicationSuffix>
--                >>=? ( <annotationSuffix> 
--                     | <noBinderFunTypeSuffix>
--                     )
--            | (<variable> | <dottedName> | <parenTerm> | <typeType>
--                  | <caseExp> | <recordCon> | <recordType> | <stringType>
--                  | <stringVal>)
--                >>=? <recordProjSuffix>
--                >>=? <applicationSuffix>
--                >>=? ( <annotationSuffix>
--                     | <noBinderFunTypeSuffix>
--                     )
--   <annotationSuffix> ::= ":" <annRight>
--   <noBinderFunTypeSuffix> ::= "->" <funRet>
--   <applicationSuffix> ::= <appArg>+
--   <recordProjSuffix> ::= ("." <varName>)+
--   <parenTerm> ::= "(" <term> ")"
--   <variable> ::= <varName>\"_"
--   <dottedName> ::= <decName> "." <varName>
--   <typeType> ::= "Type"
--   <stringType> ::= "String"
--   <stringVal> ::= <stringLit>
--   <explFunType> ::= "(" <varName>+ ":" <term> ")" "->" <funRet>
--   <implFunType> ::= "{" <varName>+ ":" <term> "}" "->" <funRet>
--   <binderFunType> ::= <explFunType> | <implFunType>
--   <explArg> ::= <varName>
--   <implArg> ::= "{" <varName> "}"
--   <lambdaArg> ::= <explArg> | <implArg>
--   <lambda> ::= "\\" <lambdaArg>+ "->" <lamBody>
--   <bareCon> ::= <decName>
--   <dottedCon> ::= <decName> "." <decName>
--   <constructor> ::= <dottedCon> | <bareCon>
--   <noArgConData> ::= <constructor>
--   <conData> ::= <constructor> <conArg>*
--   <assertionPattern> ::= "." <assertionPatternArg>
--   <varPattern> ::= <varName>
--   <noArgConPattern> ::= <constructor>
--   <conPattern> ::= <constructor> <conPatternArg>*
--   <parenPattern> ::= "(" <pattern> ")"
--   <pattern> ::= <assertionPattern>
--               | <parenPattern>
--               | <conPattern>
--               | <varPattern>
--   <consMotivePart> ::= "(" <varName>+ ":" <term> ")" "||" <caseMotiveParts>
--   <nilMotivePart> ::= <term>
--   <caseMotive> ::= <consMotivePart> | <nilMotivePart>
--   <clause> ::= (<pattern> $1 "||") "->" <term>
--   <caseExp> ::= "case" (<caseArg> $1 "||") "motive" <caseMotive>
--                 "of" "|"? (<clause> $ "|") "end"
--   <recordType> ::= "Rec" "{" (<fieldDecl> $ ",") "}"
--   <fieldDecl> ::= (<varName>\"_") ":" <term>
--   <emptyRecordCon> ::= "{" "}"
--   <nonEmptyRecordCon> ::= "{" ((<varName>\"_") "=" $ ",") "}"
--   <recordCon> ::= <emptyRecordCon> | <nonEmptyRecordCon>
--   <bareQuotedType> ::= "Quote" <quotedTtpeArg>
--   <annotatedQuotedType> ::= "Quote" "[" (<varName> $ ",") "]"
--                             <quotedTypeArg>
--   <quotedType> ::= <bareQuotedType> | <annotatedQuotedType>
--   <quote> ::= "`" <quoteArg>
--   <unquote> ::= "~" <unquoteArg>
--   <continue> ::= "continue" <continueArg>
--   <shift> ::= "shift" <varName> "in" <shiftArg>
--   <reset> ::= "reset" <varName> "in" <resetArg>
--   <require> ::= "require" <varName> ":" <requireType> "in" <requireBody>
--   <catMagic> ::= "[cat|" (<forallCategory> | <category>) "|]"
--   <forallCategory> ::= "forall"
--                          ("(" <varName>+ ":" <term> ")")+ "." <category>
--   <category> ::= <cat>
--   <cat> ::= (<bareCat> | <parensCat>)
--               >>=? ( <underCatSuffix>
--                    | <overCatSuffix>
--                    | <gapCatSuffix>
--                    )
--   <bareCat> ::= <conData>
--               | <continue>
--               | <quotedType>
--               | <quote>
--               | <unquote>
--               | <variable>
--               | <dottedName>
--               | <parenTerm>
--               | <typeType>
--               | <caseExp>
--               | <recordCon>
--               | <recordType>
--               | <stringType>
--               | <stringVal>
--   <parensCat> ::= "(" <cat> ")"
--   <underCatSuffix> ::= "\\" <underCatRet>
--   <overCatSuffix> ::= "//" <overCatArg>
--   <gapCatSuffix> ::= ">" <gapCatRet>
--   <ruleMagic> ::= "[rule|" (<forallRule> | <rule>) "|]"
--   <forallRule> ::= "forall"
--                      ("(" <varName>+ ":" <term> ")")+ "." <rule>
--   <rule> ::= (<ruleCat> $1 "*") "=>" <ruleCat>
--   <ruleCat> ::= 
--   <annRight> ::= <binderFunType>
--                | <lambda>
--                | <require>
--                | <reset>
--                | <catMagic>
--                | <ruleMagic>
--                | (<conData> | <quotedType> | <continue>)
--                    >>=? <noBinderFunTypeSuffix>
--                | (<quote> | <unquote>)
--                    >>=? <applicationSuffix>
--                    >>=? <noBinderFunTypeSuffix>
--                | (<parenTerm> | <variable> | <dottedName> | <recordType>
--                      | <recordCon> | <stringType> | <stringVal>
--                      | <typeType>)
--                    >>=? <recordProjSuffix>
--                    >>=? <applicationSuffix>
--                    >>=? <noBinderFunTypeSuffix>
--   <funRet> ::= <term>
--   <lamBody> ::= <term>
--   <explAppArg> ::= <noArgConData>
--                  | <quote>
--                  | <unquote>
--                  | (<parenTerm> | <variable> | <dottedName>
--                        | <recordType> | <recordCon> | <stringType>
--                        | <stringVal> | <typeType>)
--                      >>=? <recordProjSuffix>
--   <implAppArg> ::= "{" <term> "}"
--   <appArg> ::= <explAppArg> | <implAppArg>
--   <explConArg> ::= <noArgConData>
--                  | <quote>
--                  | <unquote>
--                  | (<parenTerm> | <variable> | <dottedName>
--                        | <recordType> | <recordCon> | <stringType>
--                        | <stringVal> | <typeType>)
--                      >>=? <recordProjSuffix>
--   <implConArg> ::= "{" <term> "}"
--   <conArg> ::= <explConArg> | <implConArg>
--   <caseArg> ::= <term>
--   <explConPatternArg> ::= <assertionPattern>
--                         | <parenPattern>
--                         | <noArgConPattern>
--                         | <varPattern>
--   <implConPatternArg> ::= "{" <pattern> "}"
--   <conPatternArg> ::= <explConPatternArg> | <implConPatternArg>
--   <assertionPatternArg> ::= <parenPattern>
--                           | <noArgConData>
--                           | <variable>
--                           | <typeType>
--                           | <stringType>
--                           | <stringVal>
--   <quotedTypeArg> ::= <noArgConData>
--                     | <caseExp>
--                     | <quote>
--                     | <unquote>
--                     | (<parenTerm> | <variable> | <dottedName>
--                           | <recordType> | <recordCon> | <stringType>
--                           | <stringVal> | <typeType>)
--                         >>=? <recordProjSuffix>
--   <quoteArg> ::= <noArgConData>
--                | <caseExp>
--                | (<patternTerm> | <variable> | <dottedName> | <recordType>
--                      | <recordCon> | <stringType> | <stringVal>
--                      | <typeType>)
--                    >>=? <recordProjSuffix>
--   <unquoteArg> ::= <noArgConData>
--                  | <caseExp>
--                  | (<patternTerm> | <variable> | <dottedName>
--                        | <recordType> | <recordCon> | <stringType> 
--                        | <stringVal> | <typeType>)
--                      >>=? <recordProjSuffix>
--   <continueArg> ::= <noArgConData>
--                   | <caseExp>
--                   | <quote>
--                   | <unquote>
--                   | (<patternTerm> | <variable> | <dottedName>
--                         | <recordType> | <recordCon> | <stringType>
--                         | <stringVal> | <typeType>)
--                       >>=? <recordProjSuffix>
--   <shiftArg> ::= <term>
--   <resetArg> ::= <term>
--   <requireType> ::= <conData>
--                   | <caseExp>
--                   | <quotedType>
--                   | <continue>
--                   | (<quote> | <unquote>)
--                       >>=? <applicationSuffix>
--                   | (<parenTerm> | <variable> | <dottedName>
--                         | <recordType> | <recordCon> | <stringType> 
--                         | <stringVal> | <typeType>)
--                       >>=? <recordProjSuffix>
--                       >>=? <applicationSuffix>
--   <requireBody> ::= <term>
--   <underCatRet> ::= <conData> | <continue> | <quotedType> | <quote>
--                   | <unquote> | <variable> | <dottedName> | <parenTerm>
--                   | <typeType> | <caseExp> | <recordCon> | <recordType>
--                   | <stringType> | <stringVal>
--   <overCatArg> ::= <conData> | <continue> | <quotedType> | <quote>
--                  | <unquote> | <variable> | <dottedName> | <parenTerm>
--                  | <typeType> | <caseExp> | <recordCon> | <recordType>
--                  | <stringType> | <stringVal>
--   <gapCatRet> ::= <conData> | <continue> | <quotedType> | <quote>
--                 | <unquote> | <variable> | <dottedName> | <parenTerm>
--                 | <typeType> | <caseExp> | <recordCon> | <recordType>
--                 | <stringType> | <stringVal>
-- @

term :: Parsec String u Term
term =
      binderFunType
  <|> lambda
  <|> require
  <|> reset
  <|> shift
  <|> catMagic
  <|> ruleMagic
  <|> (conData <|> continue <|> quotedType)
        >>=? (annotationSuffix <|>? noBinderFunTypeSuffix)
  <|> (quote <|> unquote)
        >>=? applicationSuffix
        >>=? (annotationSuffix <|>? noBinderFunTypeSuffix)
  <|> (variable <|> dottedName <|> parenTerm <|> typeType <|> caseExp
           <|> recordCon <|> recordType <|> stringType <|> stringVal <|> hole)
        >>=? recordProjSuffix
        >>=? applicationSuffix
        >>=? (annotationSuffix <|>? noBinderFunTypeSuffix)

annotationSuffix :: Term -> Parsec String u Term
annotationSuffix m =
  do try $ reservedOp ":"
     t <- annRight
     return $ annH m t

noBinderFunTypeSuffix :: Term -> Parsec String u Term
noBinderFunTypeSuffix arg =
  do try $ reservedOp "->"
     ret <- funRet
     let xsFreshDummies =
           unBNSString
             (dummiesToFreshNames
                (freeVarNames ret)
                (BNSString "_"))
     return $ funH Expl xsFreshDummies arg ret

applicationSuffix :: Term -> Parsec String u Term
applicationSuffix f =
  do pas <- many1 appArg
     return $ foldl' (\f' (plic,a') -> appH plic f' a') f pas

recordProjSuffix :: Term -> Parsec String u Term
recordProjSuffix m =
  do try $ reservedOp "."
     f <- varName
     fs <- many $ do
       _ <- reservedOp "."
       varName
     return $ foldl' recordProjH m (f:fs)

parenTerm :: Parsec String u Term
parenTerm = parens term

variable :: Parsec String u Term
variable =
  do x <- varName
     guard (x /= "_")
     return $ Var (Free (FreeVar x))

dottedName :: Parsec String u Term
dottedName =
  try $ do
    modName <- decName
    reservedOp "."
    valName <- varName
    return $ In (Defined (DottedLocal modName valName))

typeType :: Parsec String u Term
typeType =
  do reserved "Type"
     return $ In Type

stringType :: Parsec String u Term
stringType =
  do reserved "String"
     return $ In Str

stringVal :: Parsec String u Term
stringVal =
  do s <- stringLiteral
     return $ In (MkStr s)

explFunType :: Parsec String u Term
explFunType =
  do (xs,arg) <- try $ do
       (xs,arg) <- parens $ do
         xs <- many1 varName
         reservedOp ":"
         arg <- term
         return (xs,arg)
       reservedOp "->"
       return (xs,arg)
     ret <- funRet
     let xsFreshDummies =
           map unBNSString
               (dummiesToFreshNames
                  (freeVarNames ret)
                  (map BNSString xs))
     return $ helperFold (\x -> funH Expl x arg)
                         xsFreshDummies
                         ret

implFunType :: Parsec String u Term
implFunType =
  do (xs,arg) <- try $ do
       (xs,arg) <- braces $ do
         xs <- many1 varName
         reservedOp ":"
         arg <- term
         return (xs,arg)
       reservedOp "->"
       return (xs,arg)
     ret <- funRet
     let xsFreshDummies =
           map unBNSString
               (dummiesToFreshNames
                  (freeVarNames ret)
                  (map BNSString xs))
     return $ helperFold (\x -> funH Impl x arg)
                         xsFreshDummies
                         ret

binderFunType :: Parsec String u Term
binderFunType = explFunType <|> implFunType

explArg :: Parsec String u (Plicity,String)
explArg =
  do x <- varName
     return (Expl,x)

implArg :: Parsec String u (Plicity,String)
implArg =
  do x <- braces varName
     return (Impl,x)

lambdaArg :: Parsec String u (Plicity,String)
lambdaArg = explArg <|> implArg

lambda :: Parsec String u Term
lambda =
  do xs <- try $ do
       reservedOp "\\"
       many1 lambdaArg
     reservedOp "->"
     b <- lamBody
     let xsFreshDummies =
           map (\(plic,s) -> (plic, unBNSString s))
               (dummiesToFreshNames
                  (freeVarNames b)
                  (map (\(plic,s) -> (plic, BNSString s)) xs))
     return $ helperFold (\(plic,x) -> lamH plic x)
                         xsFreshDummies
                         b

bareCon :: Parsec String u Name
bareCon =
  do conName <- decName
     return $ BareLocal conName

dottedCon :: Parsec String u Name
dottedCon =
  try $ do
    modName <- decName
    reservedOp "."
    conName <- decName
    return $ DottedLocal modName conName

constructor :: Parsec String u Name
constructor = dottedCon <|> bareCon

noArgConData :: Parsec String u Term
noArgConData =
  do c <- constructor
     return $ conH c []

conData :: Parsec String u Term
conData =
  do c <- constructor
     as <- many conArg
     return $ conH c as

assertionPattern :: Parsec String u Pattern
assertionPattern = do reservedOp "."
                      m <- assertionPatternArg
                      return $ assertionPatH m

varPattern :: Parsec String u Pattern
varPattern =
  do x <- varName
     return $ Var (Free (FreeVar x))

noArgConPattern :: Parsec String u Pattern
noArgConPattern =
  do c <- constructor
     return $ conPatH c []

conPattern :: Parsec String u Pattern
conPattern =
  do c <- constructor
     ps <- many conPatternArg
     return $ conPatH c ps

parenPattern :: Parsec String u Pattern
parenPattern = parens pattern

pattern :: Parsec String u Pattern
pattern =
      assertionPattern
  <|> parenPattern
  <|> conPattern
  <|> varPattern

consMotivePart :: Parsec String u ([String], [Term], Term)
consMotivePart =
  do (xs,a) <- try $ parens $ do
       xs <- many1 varName
       reservedOp ":"
       a <- term
       return (xs,a)
     reservedOp "||"
     (xs',as,b) <- caseMotiveParts
     return (xs ++ xs', replicate (length xs) a ++ as, b)

nilMotivePart :: Parsec String u ([String], [Term], Term)
nilMotivePart =
  do b <- term
     return ([], [], b)

caseMotiveParts :: Parsec String u ([String], [Term], Term)
caseMotiveParts =
      consMotivePart
  <|> nilMotivePart

caseMotive :: Parsec String u CaseMotive
caseMotive =
  do (xs,as,b) <- caseMotiveParts
     let xsFreshDummies =
           map unBNSString
               (dummiesToFreshNames
                  (freeVarNames b ++ (freeVarNames =<< as))
                  (map BNSString xs))
     return $ caseMotiveH xsFreshDummies as b

clause :: Parsec String u Clause
clause =
  do ps <- try $ do
       ps <- pattern `sepBy1` reservedOp "||"
       reservedOp "->"
       return ps
     b <- term
     let freshenedPs =
           dummiesToFreshNames (freeVarNames b) ps
         xs = freeVarNames =<< freshenedPs
     return $ clauseH xs freshenedPs b

caseExp :: Parsec String u Term
caseExp =
  do reserved "case"
     ms <- caseArg `sepBy1` reservedOp "||"
     reserved "motive"
     mot <- caseMotive
     reserved "of"
     optional (reservedOp "|")
     cs <- clause `sepBy` reservedOp "|"
     reserved "end"
     return $ caseH ms mot cs

recordType :: Parsec String u Term
recordType =
  do reserved "Rec"
     xts <- braces $ fieldDecl `sepBy` reservedOp ","
     return $ recordTypeH xts
  where
    fieldDecl =
      do x <- varName
         guard (x /= "_")
         reservedOp ":"
         t <- term
         return (x,t)

emptyRecordCon :: Parsec String u Term
emptyRecordCon =
  try $ do
    braces $ return ()
    return $ recordConH []

nonEmptyRecordCon :: Parsec String u Term
nonEmptyRecordCon =
  do x <- try $ do
       _ <- symbol "{"
       x <- varName
       reservedOp "="
       return x
     guard (x /= "_")
     m <- term
     xms' <- many $ do
       reservedOp ","
       x' <- varName
       guard (x /= "_")
       reservedOp "="
       m' <- term
       return (x',m')
     _ <- symbol "}"
     return $ recordConH ((x,m):xms')

recordCon :: Parsec String u Term
recordCon =
      emptyRecordCon
  <|> nonEmptyRecordCon

bareQuotedType :: Parsec String u Term
bareQuotedType =
  do _ <- try $ reserved "Quoted"
     a <- quotedTypeArg
     return $ quotedTypeH [] a

annotatedQuotedType :: Parsec String u Term
annotatedQuotedType =
  do _ <- try $ do
            reserved "Quoted"
            symbol "["
     resets <- sepBy varName (reservedOp ",")
     _ <- symbol "]"
     a <- quotedTypeArg
     return $ quotedTypeH resets a

quotedType :: Parsec String u Term
quotedType =
      annotatedQuotedType
  <|> bareQuotedType

quote :: Parsec String u Term
quote =
  do _ <- try $ reservedOp "`"
     m <- quoteArg
     return $ quoteH m

unquote :: Parsec String u Term
unquote =
  do _ <- try $ reservedOp "~"
     m <- unquoteArg
     return $ unquoteH m

continue :: Parsec String u Term
continue =
  do _ <- try $ reserved "continue"
     m <- continueArg
     return $ continueH m

shift :: Parsec String u Term
shift =
  do _ <- try $ reserved "shift"
     res <- varName
     reserved "in"
     m <- shiftArg
     return $ shiftH res m

reset :: Parsec String u Term
reset =
  do _ <- try $ reserved "reset"
     res <- varName
     reserved "in"
     m <- resetArg
     return $ resetH res m

require :: Parsec String u Term
require =
  do _ <- try $ reserved "require"
     x <- varName
     reservedOp ":"
     a <- requireType
     reserved "in"
     m <- requireBody
     let xFreshDummy =
           unBNSString
             (dummiesToFreshNames
                (freeVarNames m)
                (BNSString x))
     return $ requireH xFreshDummy a m

hole :: Parsec String u Term
hole =
  do _ <- try $ symbol "?"
     n <- varName
     return $ holeH n

catMagic :: Parsec String u Term
catMagic =
  do _ <- try $ symbol "[cat|"
     r <- forallCategory <|> category
     _ <- symbol "|]"
     return r

forallCategory :: Parsec String u Term
forallCategory =
  do try $ reserved "forall"
     xsas <- many1 forallVars
     reservedOp "."
     b <- category
     let xas :: [(String,Term)]
         xas = [ (x,a) | (xs,a) <- xsas, x <- xs ]
     return $ helperFold forallCatH xas b
  where
    forallVars :: Parsec String u ([String],Term)
    forallVars =
      parens $ do
        xs <- many1 varName
        reservedOp ":"
        a <- term
        return (xs,a)
    
    forallCatH :: (String,Term) -> Term -> Term
    forallCatH (x,a) b =
      conH (Absolute "LE" "QCForall")
           [(Expl,a),(Expl,lamH Expl x b)]

category :: Parsec String u Term
category =
  do c <- cat
     return $
       conH (Absolute "LE" "QCDone") [(Expl,c)]

cat :: Parsec String u Term
cat =
  (bareCat <|> parensCat)
    >>=? (underCatSuffix <|>? overCatSuffix <|>? gapCatSuffix)

bareCat :: Parsec String u Term
bareCat =
      conData
  <|> variable
  <|> dottedName

parensCat :: Parsec String u Term
parensCat = parens cat

underCatSuffix :: Term -> Parsec String u Term
underCatSuffix arg =
  do try $ reservedOp "\\\\"
     ret <- underCatRet
     return $
       conH (Absolute "LE" "Under")
            [(Expl,arg),(Expl,ret)]

overCatSuffix :: Term -> Parsec String u Term
overCatSuffix ret =
  do try $ reservedOp "//"
     arg <- overCatArg
     return $
       conH (Absolute "LE" "Over")
            [(Expl,ret),(Expl,arg)]

gapCatSuffix :: Term -> Parsec String u Term
gapCatSuffix ret =
  do try $ reservedOp ">"
     arg <- overCatArg
     return $
       conH (Absolute "LE" "Gap")
            [(Expl,ret),(Expl,arg)]

ruleMagic :: Parsec String u Term
ruleMagic =
  do _ <- try $ symbol "[rule|"
     r <- forallRule <|> rule
     _ <- symbol "|]"
     return r

forallRule :: Parsec String u Term
forallRule =
  do try $ reserved "forall"
     xsas <- many1 forallVars
     reservedOp "."
     b <- rule
     let xas :: [(String,Term)]
         xas = [ (x,a) | (xs,a) <- xsas, x <- xs ]
     return $ helperFold forallRuleH xas b
  where
    forallVars :: Parsec String u ([String],Term)
    forallVars =
      parens $ do
        xs <- many1 varName
        reservedOp ":"
        a <- term
        return (xs,a)
    
    forallRuleH :: (String,Term) -> Term -> Term
    forallRuleH (x,a) b =
      conH (Absolute "LE" "QRForall")
           [(Expl,a),(Expl,lamH Expl x b)]

rule :: Parsec String u Term
rule =
  do args <- sepBy1 ruleCatArgRet (reservedOp "*")
     reservedOp "=>"
     ret <- ruleCatArgRet
     let rle =
           helperFold
             ruleH
             args
             (conH (Absolute "LE" "RDone") [(Expl,ret)])
     return $
       conH (Absolute "LE" "QRDone") [(Expl,rle)]
  where
    ruleH :: Term -> Term -> Term
    ruleH a r =
      conH (Absolute "LE" "RArg") [(Expl,a),(Expl,r)]

annRight :: Parsec String u Term
annRight =
      binderFunType
  <|> lambda
  <|> require
  <|> reset
  <|> shift
  <|> catMagic
  <|> ruleMagic
  <|> (conData <|> quotedType <|> continue)
        >>=? noBinderFunTypeSuffix
  <|> (quote <|> unquote)
        >>=? applicationSuffix
        >>=? noBinderFunTypeSuffix
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix
        >>=? applicationSuffix
        >>=? noBinderFunTypeSuffix

funRet :: Parsec String u Term
funRet = term

lamBody :: Parsec String u Term
lamBody = term

rawExplAppArg :: Parsec String u Term
rawExplAppArg =
      noArgConData
  <|> quote
  <|> unquote
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix

explAppArg :: Parsec String u (Plicity,Term)
explAppArg =
  do m <- rawExplAppArg
     return (Expl,m)

rawImplAppArg :: Parsec String u Term
rawImplAppArg = term

implAppArg :: Parsec String u (Plicity,Term)
implAppArg =
  do m <- braces $ rawImplAppArg
     return (Impl,m)

appArg :: Parsec String u (Plicity,Term)
appArg =
      explAppArg
  <|> implAppArg

rawExplConArg :: Parsec String u Term
rawExplConArg =
      noArgConData
  <|> quote
  <|> unquote
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix

explConArg  :: Parsec String u (Plicity,Term)
explConArg =
  do m <- rawExplConArg
     return (Expl,m)

rawImplConArg :: Parsec String u Term
rawImplConArg = term

implConArg :: Parsec String u (Plicity,Term)
implConArg =
  do m <- braces $ rawImplConArg
     return (Impl,m)

conArg :: Parsec String u (Plicity,Term)
conArg =
      explConArg
  <|> implConArg

caseArg :: Parsec String u Term
caseArg = term

rawExplConPatternArg :: Parsec String u Pattern
rawExplConPatternArg =
      assertionPattern
  <|> parenPattern
  <|> noArgConPattern
  <|> varPattern

explConPatternArg :: Parsec String u (Plicity,Pattern)
explConPatternArg =
  do p <- rawExplConPatternArg
     return (Expl,p)

rawImplConPatternArg :: Parsec String u Pattern
rawImplConPatternArg = pattern

implConPatternArg :: Parsec String u (Plicity,Pattern)
implConPatternArg =
  do p <- braces $ rawImplConPatternArg
     return (Impl,p)

conPatternArg :: Parsec String u (Plicity,Pattern)
conPatternArg =
      explConPatternArg
  <|> implConPatternArg

assertionPatternArg :: Parsec String u Term
assertionPatternArg =
      parenTerm
  <|> noArgConData
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> hole

quotedTypeArg :: Parsec String u Term
quotedTypeArg =
      noArgConData
  <|> caseExp
  <|> quote
  <|> unquote
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix

quoteArg :: Parsec String u Term
quoteArg =
      noArgConData
  <|> caseExp
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix

unquoteArg :: Parsec String u Term
unquoteArg =
      noArgConData
  <|> caseExp
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix

continueArg :: Parsec String u Term
continueArg =
      noArgConData
  <|> caseExp
  <|> quote
  <|> unquote
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix

shiftArg :: Parsec String u Term
shiftArg = term

resetArg :: Parsec String u Term
resetArg = term

requireType :: Parsec String u Term
requireType =
      conData
  <|> caseExp
  <|> quotedType
  <|> continue
  <|> (quote <|> unquote)
        >>=? applicationSuffix
  <|> (parenTerm <|> variable <|> dottedName <|> recordType <|> recordCon
          <|> stringType <|> stringVal <|> typeType <|> hole)
        >>=? recordProjSuffix >>=? applicationSuffix

requireBody :: Parsec String u Term
requireBody = term

underCatRet :: Parsec String u Term
underCatRet =
      bareCat
  <|> parensCat

overCatArg :: Parsec String u Term
overCatArg =
      bareCat
  <|> parensCat

gapCatArg :: Parsec String u Term
gapCatArg =
      bareCat
  <|> parensCat

ruleCatArgRet :: Parsec String u Term
ruleCatArgRet =
      bareCat
  <|> parensCat

parseTerm :: String -> Either String Term
parseTerm str =
  case parse (whiteSpace *> term <* eof) "(unknown)" str of
    Left e -> Left (show e)
    Right p -> Right p



-- * Statement Parsers


-- | The parser 'statement' captures the grammar of statements, defined as
--
-- @
--    <statement> ::= <termDecl> | <typeDecl> | <resetDecl>
--    <letDecl> ::= "let" x=<varName> ":" t=<term>
--                  (<eqTermDeclSuffix(x,t)> | <whereTermDeclSuffix(x,t))
--    <eqTermDeclSuffix(x,t)> ::= "=" <term> "end"
--    <whereTermDeclSuffix(x,t)> ::= "where" "|"?
--                                   (<patternMatchClause(x)> $1 "|") "end"
--    <letFamilyDecl> ::= "let" "family" <varName> <typeArg>+ ":" <term> "end"
--    <letInstanceDecl> ::= "let" "instance" n=<letInstanceName> "where" "|"?
--                          (<instancePatternMatchClause(n)> $1 "|") "end"
--    <letInstanceBareName> ::= <varName>\"_"
--    <letInstanceDottedName> ::= <decName> "." <varName>
--    <letInstanceName> ::= <letInstanceBareName> | <letInstanceDottedName>
--    <instancePatternMatchClause(n)> ::= n <wherePattern>* "=" <term>
--    <patternMatchClause(n)> ::= n <wherePattern>* "=" <term>
--    <explWherePattern> ::= <assertionPattern>
--                         | <parenPattern>
--                         | <noArgConPattern>
--                         | <varPattern>
--    <implWherePattern> ::= "{" <pattern> "}"
--    <wherePattern> ::= <explWherePattern> | <implWherePattern>
--    <termDecl> ::= <letDecl> | <letFamilyDecl> | <letInstanceDecl>
--    <alternative> ::= <decName> <alternativeArg>* ":" <term>
--    <explAlternativeArg> ::= <varName>+ ":" <term>
--    <implAlternativeArg> ::= "{" <varName>+ ":" <term> "}"
--    <alternativeArg> ::= <explAlternativeArg> | <implAlternativeArg>
--    <normalTypeDecl> ::= "data" <decName> <typeArg>*
--                         (<emptyTypeDeclSuffix> | <nonEmptyTypeDeclSuffix>)
--    <emptyTypeDeclSuffix> ::= "end"
--    <nonEmptyTypeDeclSuffix> ::= "where" "|" (<alternative> $ "|") "end"
--    <explTypeArg> ::= <varName>+ ":" <term>
--    <implTypeArg> ::= "{" <varName>+ ":" <term> "}"
--    <typeArg> ::= <explTypeArg> | <implTypeArg>
--    <dataFamilyDecl> ::= "data" "family" <decName> <typeArg>* "end"
--    <dataInstanceDecle> ::= "data" "instance" <constructor> "where" "|"?
--                            (<alternative> $ "|") "end"
--    <typeDecl> ::= <normalTypeDecl> | <dataFamilyDecl> | <dataInstanceDecl>
--    <resetDecl> ::= "reset" <varName> "from" <term> "to" <term> "end"
-- @

statement :: Parsec String u Statement
statement =
     TmDecl <$> termDecl
 <|> TyDecl <$> typeDecl
 <|> ResetDecl <$> resetDecl

letDecl :: Parsec String u TermDeclaration
letDecl =
  do x <- try $ do
       reserved "let"
       x <- varName
       return x
     reservedOp ":"
     t <- term
     eqTermDeclSuffix x t <|> whereTermDeclSuffix x t

eqTermDeclSuffix :: String -> Term -> Parsec String u TermDeclaration
eqTermDeclSuffix x t =
  do reservedOp "="
     m <- term
     reserved "end"
     return $ TermDeclaration x t m

whereTermDeclSuffix :: String -> Term -> Parsec String u TermDeclaration
whereTermDeclSuffix x t =
  do reserved "where"
     optional (reservedOp "|")
     preclauses <- patternMatchClause x `sepBy1` reservedOp "|"
     reserved "end"
     return $ WhereDeclaration x t preclauses

letFamilyDecl :: Parsec String u TermDeclaration
letFamilyDecl =
  do try $ do
       reserved "let"
       reserved "family"
       return ()
     x <- varName
     args <- typeArgs
     reservedOp ":"
     t <- term
     reserved "end"
     return $ LetFamilyDeclaration x args t

letInstanceDecl :: Parsec String u TermDeclaration
letInstanceDecl =
  do try $ do
       reserved "let"
       reserved "instance"
       return ()
     n <- letInstanceName
     reserved "where"
     optional (reservedOp "|")
     preclauses <- instancePatternMatchClause n `sepBy1` reservedOp "|"
     reserved "end"
     return $ LetInstanceDeclaration n preclauses

letInstanceBareName :: Parsec String u Name
letInstanceBareName =
  do x <- varName
     guard (x /= "_")
     return $ BareLocal x

letInstanceDottedName :: Parsec String u Name
letInstanceDottedName =
  try $ do
    modName <- decName
    reservedOp "."
    valName <- varName
    return $ DottedLocal modName valName

letInstanceName :: Parsec String u Name
letInstanceName =
      letInstanceDottedName
  <|> letInstanceBareName

instancePatternMatchClause
  :: Name -> Parsec String u ([Plicity], ([String], [Pattern], Term))
instancePatternMatchClause c
  = do c' <- letInstanceName
       guard (c == c')
       ps <- many wherePattern
       reservedOp "="
       b <- term
       let freshenedPs = dummiesToFreshNames (freeVarNames b) ps
           xs = do (_,p) <- freshenedPs
                   freeVarNames p
       return ( map fst freshenedPs
              , (xs, map snd freshenedPs, b)
              )

patternMatchClause
  :: String -> Parsec String u ([Plicity], ([String], [Pattern], Term))
patternMatchClause x =
  do _ <- symbol x
     ps <- many wherePattern
     reservedOp "="
     b <- term
     let freshenedPs =
           dummiesToFreshNames (freeVarNames b) ps
         xs = do (_,p) <- freshenedPs
                 freeVarNames p
     return ( map fst freshenedPs
            , (xs, map snd freshenedPs, b)
            )

rawExplWherePattern :: Parsec String u Pattern
rawExplWherePattern =
      assertionPattern
  <|> parenPattern
  <|> noArgConPattern
  <|> varPattern

explWherePattern :: Parsec String u (Plicity,Pattern)
explWherePattern =
  do p <- rawExplWherePattern
     return (Expl,p)

rawImplWherePattern :: Parsec String u Pattern
rawImplWherePattern = pattern

implWherePattern :: Parsec String u (Plicity,Pattern)
implWherePattern =
  do p <- braces $ rawImplWherePattern
     return (Impl,p)

wherePattern :: Parsec String u (Plicity,Pattern)
wherePattern =
      implWherePattern
  <|> explWherePattern

termDecl :: Parsec String u TermDeclaration
termDecl =
      letDecl
  <|> letFamilyDecl
  <|> letInstanceDecl

alternative :: Parsec String u (String, ConSig)
alternative =
  do c <- decName
     as <- alternativeArgs
     reservedOp ":"
     t <- term
     return (c,conSigH as t)

explAlternativeArg :: Parsec String u [DeclArg]
explAlternativeArg =
  parens $ do
    xs <- many1 varName
    reservedOp ":"
    t <- term
    return $ [ DeclArg Expl x t | x <- xs ]

implAlternativeArg :: Parsec String u [DeclArg]
implAlternativeArg =
  braces $ do
    xs <- many1 varName
    reservedOp ":"
    t <- term
    return $ [ DeclArg Impl x t | x <- xs ]

alternativeArg :: Parsec String u [DeclArg]
alternativeArg = explAlternativeArg <|> implAlternativeArg

alternativeArgs :: Parsec String u [DeclArg]
alternativeArgs =
  do argss <- many alternativeArg
     return (concat argss)

normalTypeDecl :: Parsec String u TypeDeclaration
normalTypeDecl =
  do (tycon,tyargs) <- try $ do
       reserved "data"
       tycon <- decName
       tyargs <- typeArgs
       return (tycon,tyargs)
     emptyTypeDeclSuffix tycon tyargs <|> nonEmptyTypeDeclSuffix tycon tyargs

emptyTypeDeclSuffix :: String -> [DeclArg] -> Parsec String u TypeDeclaration
emptyTypeDeclSuffix tycon tyargs =
  do reserved "end"
     return $ TypeDeclaration tycon tyargs []

nonEmptyTypeDeclSuffix
  :: String -> [DeclArg] -> Parsec String u TypeDeclaration
nonEmptyTypeDeclSuffix tycon tyargs =
  do reserved "where"
     optional (reservedOp "|")
     alts <- alternative `sepBy` reservedOp "|"
     reserved "end"
     return $ TypeDeclaration tycon tyargs alts

explTypeArg :: Parsec String u [DeclArg]
explTypeArg =
  parens $ do
    xs <- many1 varName
    reservedOp ":"
    t <- term
    return $ [ DeclArg Expl x t | x <- xs ]

implTypeArg :: Parsec String u [DeclArg]
implTypeArg =
  braces $ do
    xs <- many1 varName
    reservedOp ":"
    t <- term
    return $ [ DeclArg Impl x t | x <- xs ]

typeArg :: Parsec String u [DeclArg]
typeArg =
      explTypeArg
  <|> implTypeArg

typeArgs :: Parsec String u [DeclArg]
typeArgs =
  do argss <- many typeArg
     return (concat argss)

dataFamilyDecl :: Parsec String u TypeDeclaration
dataFamilyDecl =
  do try $ do
       reserved "data"
       reserved "family"
       return ()
     tycon <- decName
     tyargs <- typeArgs
     reserved "end"
     return $ DataFamilyDeclaration tycon tyargs

dataInstanceDecl :: Parsec String u TypeDeclaration
dataInstanceDecl =
  do try $ do
       reserved "data"
       reserved "instance"
       return ()
     tycon <- constructor
     reserved "where"
     optional (reservedOp "|")
     alts <- alternative `sepBy` reservedOp "|"
     reserved "end"
     return $ DataInstanceDeclaration tycon alts

typeDecl :: Parsec String u TypeDeclaration
typeDecl =
      normalTypeDecl
  <|> dataFamilyDecl
  <|> dataInstanceDecl

resetDecl :: Parsec String u ResetDeclaration
resetDecl =
  do _ <- try $ reserved "reset"
     res <- varName
     reserved "from"
     a <- term
     reserved "to"
     b <- term
     reserved "end"
     return $ ResetDeclaration res a b







-- * Open Settings

-- | The 'openSettings' parser captures the grammar for module opening, as
-- defined in
--
-- @
--    <openSettings> ::= <decName> <oAs>? <oHidingUsing>? <oRenaming>?
--    <oAs> ::= "as" <decName>
--    <oHidingUsing> ::= <hiding> | <using>
--    <hiding> ::= "hiding" "(" ((<varName> | <decName>) $ ",") ")"
--    <using> ::= "using" "(" ((<varName> | <decName>) $ ",") ")"
--    <oRenaming> ::= "renaming" "(" ((<varRen> | <decRen>) $ ",") ")"
--    <varRen> ::= <varName> "to" <varName>
--    <decRen> ::= <decName> "to" <decName>
-- @

openSettings :: Parsec String u OpenSettings
openSettings =
  OpenSettings
    <$> decName
    <*> oAs
    <*> oHidingUsing
    <*> oRenaming

oAs :: Parsec String u (Maybe String)
oAs =
  optionMaybe $ do
    reserved "as"
    decName

oHidingUsing :: Parsec String u (Maybe HidingUsing)
oHidingUsing = optionMaybe (hiding <|> using)
  where
    hiding :: Parsec String u HidingUsing
    hiding =
      do reserved "hiding"
         ns <- parens (sepBy (varName <|> decName) (reservedOp ","))
         return (Hiding ns)
    
    using :: Parsec String u HidingUsing
    using =
      do reserved "using"
         ns <- parens (sepBy (varName <|> decName) (reservedOp ","))
         return (Using ns)

oRenaming :: Parsec String u [(String,String)]
oRenaming =
  do m <- openRenamingP
     case m of
       Nothing -> return []
       Just ns -> return ns
  where
    openRenamingP :: Parsec String u (Maybe [(String,String)])
    openRenamingP =
      optionMaybe $ do
        reserved "renaming"
        parens (sepBy (varRen <|> decRen) (reservedOp ","))
    
    varRen :: Parsec String u (String,String)
    varRen =
      do n <- varName
         reserved "to"
         n' <- varName
         return (n,n')
    
    decRen :: Parsec String u (String,String)
    decRen =
      do n <- decName
         reserved "to"
         n' <- decName
         return (n,n')






-- * Modules

-- | The parser 'modul' captures the grammar for modules defined as
--
-- @
--    <module> ::= "module" <decName> ("opening" (<openSettings> $ "|"))?
--                 "where" <statement>* "end"
-- @

modul :: Parsec String u Module
modul =
  do try $ reserved "module"
     n <- decName
     settings <- option [] $ do
       try $ reserved "opening"
       optional (reservedOp "|")
       openSettings `sepBy` reservedOp "|"
     reserved "where"
     stmts <- many statement
     reserved "end"
     return $ Module n settings stmts





-- * Programs

-- | A 'program' is just many modules.
--
-- @
--    <program> ::= <module>*
-- @

program :: Parsec String u Program
program = Program <$> many modul





parseProgram :: String -> String -> Either String Program
parseProgram fn str
  = case parse (whiteSpace *> program <* eof) fn str of
      Left err ->
        Left (show err)
      Right p -> Right p