{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golem.Core.Parser where

import Control.Monad.Reader
import Data.List (foldl')
import Text.Parsec
import Text.Parsec.Error
import qualified Text.Parsec.Token as Token

import Golem.Utils.ABT hiding (shift)
import Golem.Utils.Names
import Golem.Utils.Plicity
import Golem.Utils.Vars

import Golem.Core.ConSig
import Golem.Core.DeclArg
import Golem.Core.Term
import Golem.Core.Program

import Debug







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
                  ]
              , Token.reservedOpNames =
                  ["|","||","->","\\",":","::","=",".","`","~"]
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


-- term parsers

term :: Parsec String u Term
term =
      binderFunType
  <|> lambda
  <|> require
  <|> reset
  <|> shift
  <|> debugSeq "A" ((conData <|> continue <|> quotedType)
        >>=? (annotationSuffix <|>? noBinderFunTypeSuffix))
  <|> debugSeq "B" ((quote <|> unquote)
        >>=? applicationSuffix
        >>=? (annotationSuffix <|>? noBinderFunTypeSuffix))
  <|> debugSeq "C" ((debugSeq "C1" variable <|> debugSeq "C2" dottedName <|> debugSeq "C3" parenTerm <|> debugSeq "C4" typeType <|> debugSeq "C5" caseExp
           <|> debugSeq "C6" recordCon <|> debugSeq "C7" recordType <|> debugSeq "C8" stringType <|> debugSeq "C9" stringVal)
        >>=? recordProjSuffix
        >>=? applicationSuffix
        >>=? (annotationSuffix <|>? noBinderFunTypeSuffix))
  
{-}
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon
  -}

annotationSuffix :: Term -> Parsec String u Term
annotationSuffix m =
  do try $ reservedOp ":"
     t <- annRight
     return $ annH m t

noBinderFunTypeSuffix :: Term -> Parsec String u Term
noBinderFunTypeSuffix arg =
  do try $ reservedOp "->"
     ret <- term
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
parenTerm =
  do debug_ "Parens1"
     x <- parens term
     debug_ "Parens2"
     return x

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

recordProj :: Parsec String u Term
recordProj = do (m,f) <- try $ do
                  m <- recProjArg
                  _ <- reservedOp "."
                  f <- varName
                  return (m,f)
                fieldNames <- many $ do
                  _ <- reservedOp "."
                  varName
                return $ foldl' recordProjH m (f:fieldNames)

recProjArg :: Parsec String u Term
recProjArg = recordType <|> recordCon <|> dottedName <|> variable <|> parenTerm <|> typeType <|> stringType <|> stringVal

dottedThings :: Parsec String u Term
dottedThings = recordProj <|> dottedName

annotation :: Parsec String u Term
annotation =
  do m <- try $ do
       m <- annLeft
       reservedOp ":"
       return m
     t <- annRight
     return $ annH m t

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

noBinderFunType :: Parsec String u Term
noBinderFunType =
  do arg <- try $ do
       arg <- funArg
       reservedOp "->"
       return arg
     ret <- funRet
     let xsFreshDummies =
           unBNSString
             (dummiesToFreshNames
                (freeVarNames ret)
                (BNSString "_"))
     return $ funH Expl xsFreshDummies arg ret

funType :: Parsec String u Term
funType = binderFunType <|> noBinderFunType

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

application :: Parsec String u Term
application =
  do (f,pa) <- try $ do
       f <- appFun
       pa <- appArg
       return (f,pa)
     pas <- many appArg
     return $ foldl' (\f' (plic,a') -> appH plic f' a') f (pa:pas)

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
     reservedOp "motive"
     mot <- caseMotive
     reserved "of"
     optional (reservedOp "|")
     cs <- clause `sepBy` reservedOp "|"
     reserved "end"
     return $ caseH ms mot cs

recordType :: Parsec String u Term
recordType =
  do reserved "Rec"
     xts <- braces $ debugSeq "fields" (fieldDecl `sepBy` reservedOp ",")
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
     resets <- many varName
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

annLeft :: Parsec String u Term
annLeft =
      application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

annRight :: Parsec String u Term
annRight =
      funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

funArg :: Parsec String u Term
funArg =
      application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

funRet :: Parsec String u Term
funRet =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

lamBody :: Parsec String u Term
lamBody =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

appFun :: Parsec String u Term
appFun =
      dottedThings
  <|> parenTerm
  <|> quote
  <|> unquote
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

rawExplAppArg :: Parsec String u Term
rawExplAppArg =
      dottedThings
  <|> parenTerm
  <|> noArgConData
  <|> quote
  <|> unquote
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

explAppArg :: Parsec String u (Plicity,Term)
explAppArg =
  do m <- rawExplAppArg
     return (Expl,m)

rawImplAppArg :: Parsec String u Term
rawImplAppArg =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

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
      dottedThings
  <|> parenTerm
  <|> noArgConData
  <|> quote
  <|> unquote
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

explConArg  :: Parsec String u (Plicity,Term)
explConArg =
  do m <- rawExplConArg
     return (Expl,m)

rawImplConArg :: Parsec String u Term
rawImplConArg =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

implConArg :: Parsec String u (Plicity,Term)
implConArg =
  do m <- braces $ rawImplConArg
     return (Impl,m)

conArg :: Parsec String u (Plicity,Term)
conArg =
      explConArg
  <|> implConArg

caseArg :: Parsec String u Term
caseArg =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

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
rawImplConPatternArg =
      assertionPattern
  <|> parenPattern
  <|> conPattern
  <|> varPattern

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

quotedTypeArg :: Parsec String u Term
quotedTypeArg =
      dottedThings
  <|> parenTerm
  <|> noArgConData
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> quote
  <|> unquote
  <|> recordType
  <|> recordCon

quoteArg :: Parsec String u Term
quoteArg =
      dottedThings
  <|> parenTerm
  <|> noArgConData
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

unquoteArg :: Parsec String u Term
unquoteArg =
      dottedThings
  <|> parenTerm
  <|> noArgConData
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

continueArg :: Parsec String u Term
continueArg =
      dottedThings
  <|> parenTerm
  <|> noArgConData
  <|> quote
  <|> unquote
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

shiftArg :: Parsec String u Term
shiftArg =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

resetArg :: Parsec String u Term
resetArg =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

requireType :: Parsec String u Term
requireType =
      application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

requireBody :: Parsec String u Term
requireBody =
      annotation
  <|> funType
  <|> application
  <|> continue
  <|> dottedThings
  <|> parenTerm
  <|> lambda
  <|> shift
  <|> reset
  <|> require
  <|> conData
  <|> quotedType
  <|> quote
  <|> unquote
  <|> caseExp
  <|> variable
  <|> typeType
  <|> stringType
  <|> stringVal
  <|> recordType
  <|> recordCon

parseTerm :: String -> Either String Term
parseTerm str =
  case parse (whiteSpace *> term <* eof) "(unknown)" str of
    Left e -> Left (show e)
    Right p -> Right p



-- statement parsers

eqTermDecl :: Parsec String u TermDeclaration
eqTermDecl =
  do (x,t) <- try $ do
       reserved "let"
       x <- varName
       reservedOp ":"
       t <- term
       reservedOp "="
       return (x,t)
     m <- term
     reserved "end"
     return $ TermDeclaration x t m

whereTermDecl :: Parsec String u TermDeclaration
whereTermDecl =
  do (x,t) <- try $ do
       reserved "let"
       x <- varName
       reservedOp ":"
       t <- term
       reserved "where"
       return (x,t)
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
rawImplWherePattern =
      assertionPattern
  <|> parenPattern
  <|> conPattern
  <|> varPattern

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
      letFamilyDecl
  <|> letInstanceDecl
  <|> eqTermDecl
  <|> whereTermDecl

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

emptyTypeDecl :: Parsec String u TypeDeclaration
emptyTypeDecl =
  do (tycon,tyargs) <- try $ do
       reserved "data"
       tycon <- decName
       tyargs <- typeArgs
       reserved "end"
       return (tycon,tyargs)
     return $ TypeDeclaration tycon tyargs []

nonEmptyTypeDecl :: Parsec String u TypeDeclaration
nonEmptyTypeDecl =
  do (tycon,tyargs) <- try $ do
       reserved "data"
       tycon <- decName
       tyargs <- typeArgs
       reserved "where"
       return (tycon,tyargs)
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
      emptyTypeDecl
  <|> nonEmptyTypeDecl
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

statement :: Parsec String u Statement
statement =
     TmDecl <$> termDecl
 <|> TyDecl <$> typeDecl
 <|> ResetDecl <$> resetDecl





-- open settings

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

openSettings :: Parsec String u OpenSettings
openSettings =
  OpenSettings
    <$> decName
    <*> oAs
    <*> oHidingUsing
    <*> oRenaming




-- modules

modulOpen :: Parsec String u Module
modulOpen =
  do n <- try $ do
       reserved "module"
       n <- decName
       reserved "opening"
       return n
     optional (reserved "|")
     settings <- sepBy openSettings (reserved "|")
     reserved "where"
     stmts <- many statement
     reserved "end"
     return $ Module n settings stmts

modulNoOpen :: Parsec String u Module
modulNoOpen =
  do n <- try $ do
       reserved "module"
       n <- decName
       reserved "where"
       return n
     stmts <- many statement
     reserved "end"
     return $ Module n [] stmts

modul :: Parsec String u Module
modul =
      modulOpen
  <|> modulNoOpen





-- programs

program :: Parsec String u Program
program = Program <$> many modul



parseProgram :: String -> Either String Program
parseProgram str
  = case parse (whiteSpace *> program <* eof) "Parse Error" str of
      Left err ->
        let pos = errorPos err
        in Left ("Parse error at line " ++ show (sourceLine pos) ++
                   ", column " ++ show (sourceColumn pos) ++ ":\n" ++
                     showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"
                            (errorMessages err))
      Right p -> Right p