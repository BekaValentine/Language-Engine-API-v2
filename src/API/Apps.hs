{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the API for apps.

module API.Apps where

import API.APITypes
import qualified API.Authorization as Auth
import APIUtils.GrammarExtraction
import qualified APIUtils.InputProcessing as IP
import qualified APIUtils.WorldModel as WM
import qualified Charted.Charted as C

import Control.Monad.Except
import Control.Monad.IO.Class ()
import Data.Aeson
import qualified Data.Binary as B
import Data.ByteString.Lazy (ByteString)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import GHC.Generics
import Servant
import System.Random







-- | An 'AppSummary' consists of the app's ID, it's name, and its description.

data AppSummary
  = AppSummary
    { appIDSummary :: AppID
    , appNameSummary :: String
    , appDescriptionSummary :: String
    }
  deriving (Generic)

instance ToJSON AppSummary





-- | An 'App' consists of its ID, name, description, tokens, and summaries of
-- the app's packages.

data App
  = App
    { appID :: AppID
    , appName :: String
    , appDescription :: String
    , tokens :: [Token]
    , packageSummaries :: [PackageSummary]
    , parseErrorSummaries :: [ParseErrorSummary]
    }
  deriving (Generic)

instance ToJSON App





-- | An 'AppConfig' consists of just the name of the app.

data AppConfig
  = AppConfig
    { appNameConfig :: String
    }
  deriving (Generic)

instance FromJSON AppConfig





-- | An 'AppUpdate' consists of an optional new name, an optional new
-- description, and an optional new list of package IDs.

data AppUpdate
  = AppUpdate
    { appNameUpdate :: Maybe String
    , appDescriptionUpdate :: Maybe String
    , appPackagesUpdate :: Maybe [PackageID]
    }
  deriving (Generic)

instance FromJSON AppUpdate





-- | A 'PackageSummary' consists of an ID, a name, an owner ID, whether or not
-- the package uses prelude, whether or not it is a prelude package, and
-- whether or not it needs to be built.

data PackageSummary
  = PackageSummary
    { packageIDSummary :: PackageID
    , packageNameSummary :: String
    , packageDescriptionSummary :: String
    , packageUsesPreludeSummary :: Bool
    , packageIsPreludeSummary :: Bool
    , packageIsPublicSummary :: Bool
    }
  deriving (Generic)

instance ToJSON PackageSummary





-- | A 'Token' consists of the token ID, and the token string.

data Token
  = Token
    { tokenID :: TokenID
    , tokenString :: String
    }
  deriving (Generic)

instance ToJSON Token





-- | A 'Conversation' consists of a conversation ID and an initial world
-- model.

data Conversation
  = Conversation
    { conversationID :: ConversationID
    }
  deriving (Generic)

instance ToJSON Conversation





-- | A 'ConversationConfig' is just a token that must belong to the app in
-- order to create a conversation.

data ConversationConfig
  = ConversationConfig
    { tokenStringConfig :: String
    }
  deriving (Generic)

instance FromJSON ConversationConfig





-- | A 'ConversationUpdate' consists of a token together with update info.

data ConversationUpdate
  = ConversationUpdate
    { tokenStringUpdate :: String
    , updateInfo :: ConversationUpdateInfo
    }
  deriving (Generic)

instance FromJSON ConversationUpdate





-- | A 'ConversationUpdateInfo' can be either a world model update, which puts
-- new entities and facts into the world mode, a world model reset, which
-- empties out the world model, and a discourse move, which consists of a
-- natural language input to process.

data ConversationUpdateInfo
  = ConversationUpdateWorldModel
    { newNextEntity :: Int
    , newFacts :: [WM.Fact]
    }
  | ConversationUpdateResetWorldModel
  | ConversationUpdateDiscourseMove
    { move :: String
    }
  deriving (Generic)

instance FromJSON ConversationUpdateInfo





-- | A 'ConversationChange' consists of an optional set of entity descriptions
-- that represent the possible response to a discourse move (if that's the
-- kind of update that caused the change).

data ConversationChange
  = ConversationChange
    { change :: Maybe WMFacts
    }
  deriving (Generic)

instance ToJSON ConversationChange





-- | A 'REPLConversationError' can be either an 'UnknownWord' error, an
-- 'IncompleteParse' error, or a 'MiscError'.

data REPLConversationError
  = UnknownWord
    { replWord :: String
    }
  | IncompleteParse
    { replChart :: [[C.BracketedSequence String String]]
    }
  | MiscError
  deriving (Generic)

instance ToJSON REPLConversationError





-- | A 'REPLConversationChange' is just like a 'ConversationChange' except it
-- can also provide information in the case of an error.

data REPLConversationChange
  = REPLConversationChange
    { replChange :: Maybe WMFacts
    }
  | REPLConversationError
    { replError :: REPLConversationError
    }
  deriving (Generic)

instance ToJSON REPLConversationChange





-- | A 'WMFacts' is just a wrapper around a pair of of new world model and
-- some new facts.

data WMFacts
  = WMFacts
    { worldModel :: WM.WorldModel
    , facts :: [WM.Fact]
    }
  deriving (Generic)

instance ToJSON WMFacts





-- | A 'ParseErrorSummary' is a representation of a parse error that is
-- more useful for the API.

data ParseErrorSummary
  = ParseErrorSummary
    { errorIDSummary :: ErrorID
    , errorTypeSummary :: String
    , errorInputSummary :: String
    , errorWordSummary :: Maybe String
    }
  deriving (Generic)

instance ToJSON ParseErrorSummary




-- | A 'ParseErrorChart' provides a representation of the chart for a parse
-- error.

data ParseErrorChart
  = ParseErrorChart
    { chart :: [[C.BracketedSequence String String]]
    }
  deriving (Generic)

instance ToJSON ParseErrorChart





-- | We can confirm that a user is authorized on an app by looking in the DB
-- for an app with the specified ID owned by the specified user.

ensureUserAuthorized :: DB.Connection
                     -> AppID
                     -> UserID
                     -> ExceptT ServantErr IO ()
ensureUserAuthorized conn aid uid =
  do foundApps :: [DB.Only AppID]
       <- liftIO $ DB.query
            conn
            "SELECT id FROM apps WHERE id = ? AND owner = ?"
            (aid,uid)
     when (null foundApps)
          (throwError err401)





-- | We can ensure that a token is valid for an app by checking the database.

ensureTokenValid :: DB.Connection
                 -> AppID
                 -> String
                 -> ExceptT ServantErr IO ()
ensureTokenValid conn aid tok =
  do foundTokens :: [DB.Only TokenID]
       <- liftIO $ DB.query
            conn
            "SELECT id FROM tokens WHERE token = ? AND app = ?"
            (tok,aid)
     when (null foundTokens)
          (throwError err401)





-- | We can confirm that a user is authorized on an app by looking in the DB
-- for an app with the specified ID owned by the specified user.

ensureUserAuthorizedOnREPLConversation
  :: DB.Connection
  -> AppID
  -> ConversationID
  -> UserID
  -> ExceptT ServantErr IO ()
ensureUserAuthorizedOnREPLConversation conn aid cid uid =
  do foundApps :: [DB.Only AppID]
       <- liftIO $ DB.query
            conn
            " SELECT apps.id                       \
            \ FROM apps, repl_conversations        \
            \ WHERE apps.id = ?                    \
            \ AND apps.owner = ?                   \
            \ AND repl_conversations.id = ?        \
            \ AND repl_conversations.app = apps.id "
            (aid,uid,cid)
     when (null foundApps)
          (throwError err401)





-- | The apps API has most of the functionality of the system. A user can list
-- their apps by @GET@ing from @/apps@, create a new app by @POST@ing an
-- 'AppConfig' to @/apps@, get info on an app by @GET@ing from @/apps/:id@,
-- update an apps info by @PUT@ing an 'AppUpdate' @/apps/:id@, and delete an
-- app by @DELETE@ing at @/apps/:id@. They can also create an app token by
-- @POST@ing to @/apps/:id/tokens@, and delete a token by @DELETE@ing at
-- @/apps/:id/tokens/:id@. They can also create a conversation by @POST@ing a
-- to @/apps/:id/conversations@, and update a conversation by @PUT@ing a
-- 'ConversationUpdate' to @/apps/:id/conversations/:id@.

type AppsAPI =
  
       "apps"
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] [AppSummary]
  
  :<|> "apps"
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] AppConfig
         :> Post '[JSON] AppSummary
  
  :<|> "apps" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] App
  
  :<|> "apps" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] AppUpdate
         :> Put '[JSON] ()
  
  :<|> "apps" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()
  
  :<|> "apps" :> CaptureID :> "tokens"
         :> BasicAuth "le-realm" Auth.Authorization
         :> Post '[JSON] Token
  
  :<|> "apps" :> CaptureID :> "tokens" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()
  
  :<|> "apps" :> CaptureID :> "conversations"
         :> ReqBody '[JSON] ConversationConfig
         :> Post '[JSON] Conversation
  
  :<|> "apps" :> CaptureID :> "conversations" :> CaptureID
         :> ReqBody '[JSON] ConversationUpdate
         :> Put '[JSON] ConversationChange
  
  :<|> "apps" :> CaptureID :> "parse-errors" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()
  
  :<|> "apps" :> CaptureID :> "parse-error-charts" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] ParseErrorChart
  
  :<|> "apps" :> CaptureID :> "repl-conversations"
         :> BasicAuth "le-realm" Auth.Authorization
         :> Post '[JSON] Conversation
  
  :<|> "apps" :> CaptureID :> "repl-conversations" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] ConversationUpdateInfo
         :> Put '[JSON] REPLConversationChange





appsServer :: DB.Connection -> Server AppsAPI
appsServer conn =
       apps_get conn
  :<|> apps_post conn
  :<|> apps_id_get conn
  :<|> apps_id_put conn
  :<|> apps_id_delete conn
  :<|> apps_id_tokens_post conn
  :<|> apps_id_tokens_id_delete conn
  :<|> apps_id_conversations_post conn
  :<|> apps_id_conversations_id_put conn
  :<|> apps_id_parse_errors_id_delete conn
  :<|> apps_id_parse_error_charts_id_get conn
  :<|> apps_id_repl_conversations_post conn
  :<|> apps_id_repl_conversations_id_put conn





-- | We can @GET@ from @/apps@ to list the apps that a user has.

apps_get :: DB.Connection
         -> Server ("apps"
                      :> BasicAuth "le-realm" Auth.Authorization
                      :> Get '[JSON] [AppSummary])
apps_get conn auth =
  do foundApps :: [(AppID,String,String)]
       <- liftIO $ DB.query
            conn
            "SELECT id, name, description FROM apps WHERE owner = ?"
            (DB.Only (Auth.userID auth))
     return [ AppSummary
              { appIDSummary = aid
              , appNameSummary = nme
              , appDescriptionSummary = desc
              }
            | (aid,nme,desc) <- foundApps
            ]





-- | We can @POST@ an 'AppConfig' to @/apps@ to create a new app.

apps_post :: DB.Connection
          -> Server ("apps"
                       :> BasicAuth "le-realm" Auth.Authorization
                       :> ReqBody '[JSON] AppConfig
                       :> Post '[JSON] AppSummary)
apps_post conn auth (AppConfig nme) =
  do [DB.Only aid] :: [DB.Only AppID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO apps \
            \   (name,owner)   \
            \ VALUES (?, ?)    \
            \ RETURNING id     "
            (nme, Auth.userID auth)
     return AppSummary
            { appIDSummary = aid
            , appNameSummary = nme
            , appDescriptionSummary = ""
            }





-- | We can @GET@ from @/apps/:id@ to get the information about an app.

apps_id_get :: DB.Connection
            -> Server ("apps" :> CaptureID
                         :> BasicAuth "le-realm" Auth.Authorization
                         :> Get '[JSON] App)
apps_id_get conn aid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     [(nme,desc,pkgs)] :: [(String,String,DB.PGArray PackageID)]
       <- liftIO $ DB.query
            conn
            "SELECT name, description, packages FROM apps WHERE id = ?"
            (DB.Only aid)
     foundPkgs :: [(PackageID,String,String,Bool,Bool,Bool)]
       <- liftIO $ DB.query
            conn
            " SELECT                                 \
            \   id, name, description, uses_prelude, \
            \   is_prelude, is_public                \
            \ FROM packages                          \
            \ WHERE id IN ?                          "
            (DB.Only (DB.In (DB.fromPGArray pkgs)))
     foundToks :: [(TokenID,String)]
       <- liftIO $ DB.query
            conn
            "SELECT id, token FROM tokens WHERE app = ?"
            (DB.Only aid)
     parseErrors :: [(ErrorID,String,ByteString)]
       <- liftIO $ DB.query
            conn
            "SELECT id,input,error FROM parse_errors WHERE app = ?"
            (DB.Only aid)
     return App
            { appID = aid
            , appName = nme
            , appDescription = desc
            , tokens =
                [ Token
                  { tokenID = tid
                  , tokenString = tok
                  }
                | (tid,tok) <- foundToks
                ]
            , packageSummaries =
                [ PackageSummary
                  { packageIDSummary = pid
                  , packageNameSummary = pnme
                  , packageDescriptionSummary = pdesc
                  , packageUsesPreludeSummary = up
                  , packageIsPreludeSummary = isp
                  , packageIsPublicSummary = ispub
                  }
                | (pid,pnme,pdesc,up,isp,ispub) <- foundPkgs
                ]
            , parseErrorSummaries =
                [ case B.decode err :: C.ParseError String String of
                    C.UnknownWord w ->
                      ParseErrorSummary
                      { errorIDSummary = eid
                      , errorTypeSummary = "UnknownWord"
                      , errorInputSummary = input
                      , errorWordSummary = Just w
                      }
                    C.IncompleteParse _ ->
                      ParseErrorSummary
                      { errorIDSummary = eid
                      , errorTypeSummary = "IncompleteParse"
                      , errorInputSummary = input
                      , errorWordSummary = Nothing
                      }
                | (eid,input,err) <- parseErrors
                ]
            }





-- | We can @PUT@ an 'AppUpdate' to @/apps/:id@ to modify an app.

apps_id_put :: DB.Connection
            -> Server ("apps" :> CaptureID
                         :> BasicAuth "le-realm" Auth.Authorization
                         :> ReqBody '[JSON] AppUpdate
                         :> Put '[JSON] ())
apps_id_put conn aid auth update =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     liftIO $ DB.withTransaction conn $ do
       updateName (appNameUpdate update)
       updateDescription (appDescriptionUpdate update)
       updatePackages (appPackagesUpdate update)
     return ()
  where
    updateName :: Maybe String -> IO ()
    updateName = mapM_ $ \nme ->
      do _ <- DB.execute
                conn
                "UPDATE apps SET name = ? WHERE id = ?"
                (nme,aid)
         return ()
    
    updateDescription :: Maybe String -> IO ()
    updateDescription = mapM_ $ \desc ->
      do _ <- DB.execute
                conn
                "UPDATE apps SET description = ? WHERE id = ?"
                (desc,aid)
         return ()
    
    updatePackages :: Maybe [PackageID] -> IO ()
    updatePackages = mapM_ $ \pids ->
      do _ <- DB.execute
                conn
                " UPDATE apps                  \
                \ SET packages = ARRAY (       \
                \       SELECT id              \
                \       FROM packages          \
                \       WHERE id = ANY (?)     \
                \       AND is_prelude = false \
                \     )                        \
                \ WHERE id = ?                 "
                (DB.PGArray pids, aid)
         return ()





-- | We can @DELETE@ at @/apps/:id@ to delete an app.

apps_id_delete :: DB.Connection
               -> Server ("apps" :> CaptureID
                            :> BasicAuth "le-realm" Auth.Authorization
                            :> Delete '[JSON] ())
apps_id_delete conn aid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM apps WHERE id = ?"
            (DB.Only aid)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM tokens WHERE app = ?"
            (DB.Only aid)
     return ()





-- | We can @POST@ to @/apps/:id/tokens@ to create a new app token.

apps_id_tokens_post
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "tokens"
               :> BasicAuth "le-realm" Auth.Authorization
               :> Post '[JSON] Token)
apps_id_tokens_post conn aid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     let syms = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
     newTokIndexes <-
       liftIO $ sequence (replicate 30 (randomRIO (0,length syms-1)))
     let newTok = [ syms !! i | i <- newTokIndexes ]
     [DB.Only tid] :: [DB.Only TokenID]
       <- liftIO $ DB.query
            conn
            "INSERT INTO tokens (token,app) VALUES (?,?) RETURNING id"
            (newTok,aid)
     return Token
            { tokenID = tid
            , tokenString = newTok
            }





-- | We can @DELETE@ at @/apps/:id/tokens/:id@ to delete an app token.

apps_id_tokens_id_delete
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "tokens" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Delete '[JSON] ())
apps_id_tokens_id_delete conn aid tid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM tokens WHERE id = ? AND app = ?"
            (tid, aid)
     return ()





-- | We can @POST@ to @/apps/:id/conversations@ to create a new conversation.

apps_id_conversations_post
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "conversations"
              :> ReqBody '[JSON] ConversationConfig
              :> Post '[JSON] Conversation)
apps_id_conversations_post conn aid (ConversationConfig tok) =
  do ensureTokenValid conn aid tok
     [DB.Only cid] :: [DB.Only ConversationID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO conversations \
            \   (app, binary_rep)       \
            \ VALUES (?, ?)             \
            \ RETURNING id              "
            (aid, DB.Binary (B.encode WM.emptyWorldModel))
     return Conversation
            { conversationID = cid
            }





-- | We can @PUT@ a 'ConversationUpdate' to @/apps/:id/conversations/:id@ to
-- update a conversation.

apps_id_conversations_id_put
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "conversations" :> CaptureID
               :> ReqBody '[JSON] ConversationUpdate
               :> Put '[JSON] ConversationChange)
apps_id_conversations_id_put conn aid cid (ConversationUpdate tok info) =
  do ensureTokenValid conn aid tok
     handleConvoUpdate info
  where
    handleConvoUpdate :: ConversationUpdateInfo
                      -> ExceptT ServantErr IO ConversationChange
    handleConvoUpdate (ConversationUpdateWorldModel newNextEnt newFcts) =
      do [DB.Only wmByteString] :: [DB.Only ByteString]
           <- liftIO $ DB.query
                conn
                "SELECT binary_rep FROM conversations WHERE id = ?"
                (DB.Only cid)
         let wm = B.decode wmByteString :: WM.WorldModel
             newWorldModel =
               WM.WorldModel
               { WM.nextEntity = max (WM.nextEntity wm) newNextEnt
               , WM.facts = newFcts ++ WM.facts wm
               }
         _ <- liftIO $ DB.execute
                conn
                "UPDATE conversations SET binary_rep = ? WHERE id = ?"
                (DB.Binary (B.encode newWorldModel), cid)
         return ConversationChange
                { change = Nothing
                }
    handleConvoUpdate ConversationUpdateResetWorldModel =
      do _ <- liftIO $ DB.execute
                conn
                "UPDATE conversations SET binary_rep = ? WHERE id = ?"
                (DB.Binary (B.encode WM.emptyWorldModel), cid)
         return ConversationChange
                { change = Nothing
                }
    handleConvoUpdate (ConversationUpdateDiscourseMove mve) =
      do [DB.Only wmByteString] :: [DB.Only ByteString]
           <- liftIO $ DB.query
                conn
                "SELECT binary_rep FROM conversations WHERE id = ?"
                (DB.Only cid)
         buildProducts :: [DB.Only ByteString]
           <- liftIO $ DB.query
                conn
                " SELECT packages.build_product         \
                \ FROM packages, apps                   \
                \ WHERE                                 \
                \   ( packages.id = ANY (apps.packages) \
                \     OR packages.is_prelude            \
                \   )                                   \
                \ AND apps.id = ?                       "
                (DB.Only aid)
         let LEExtract
               { extractEnvironment = env
               , extractWords = wds
               , extractRules = rles
               }
               = combineExtracts
                   [ B.decode exByteString
                   | DB.Only exByteString <- buildProducts
                   ]
             wm = B.decode wmByteString :: WM.WorldModel
             pinfo = IP.ProcessingInfo
                     { IP.lexicon = wds
                     , IP.grammarRules = rles
                     , IP.environment = env
                     , IP.worldModel = wm
                     }
         case IP.processInput pinfo mve of
           Left err -> case err of
             Nothing -> throwError err412
             Just parseErr ->
               do logParseError mve parseErr
                  throwError err412
           Right (newWm,fcts) ->
             do _ <- liftIO $ DB.execute
                       conn
                       "UPDATE conversations SET binary_rep = ? WHERE id = ?"
                       (DB.Binary (B.encode newWm), cid)
                return ConversationChange
                       { change = Just (WMFacts newWm fcts)
                       }
    
    logParseError :: String
                  -> C.ParseError String String
                  -> ExceptT ServantErr IO ()
    logParseError input err =
      do let binerr = B.encode err
         _ <- liftIO $ DB.execute
                conn
                " INSERT INTO parse_errors \
                \   (app,input,error)      \
                \ VALUES (?,?,?)           "
                (aid, input, DB.Binary binerr)
         return ()





apps_id_parse_errors_id_delete
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "parse-errors" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Delete '[JSON] ())
apps_id_parse_errors_id_delete conn aid eid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM parse_errors WHERE id = ? AND app = ?"
            (eid, aid)
     return ()





apps_id_parse_error_charts_id_get
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "parse-error-charts" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Get '[JSON] ParseErrorChart)
apps_id_parse_error_charts_id_get conn aid eid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     errs :: [DB.Only ByteString]
       <- liftIO $ DB.query
            conn
            "SELECT error FROM parse_errors WHERE id = ? AND app = ?"
            (eid, aid)
     case errs of
       [] -> throwError err404
       _:_:_ -> throwError err500
       [DB.Only errbs] ->
         case B.decode errbs :: C.ParseError String String of
           C.UnknownWord _ -> throwError err404
           C.IncompleteParse c ->
             return ParseErrorChart
                    { chart = C.chartToBracketedSequences c
                    }





-- | We can @POST@ to @/apps/:id/repl-conversations@ to create a new REPL
-- conversation.

apps_id_repl_conversations_post
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "repl-conversations"
              :> BasicAuth "le-realm" Auth.Authorization
              :> Post '[JSON] Conversation)
apps_id_repl_conversations_post conn aid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     [DB.Only cid] :: [DB.Only ConversationID]
       <- liftIO $ DB.query
            conn
            " DELETE FROM repl_conversations \
            \ WHERE app = ? ;                \
            \ INSERT INTO repl_conversations \
            \   (app, binary_rep)            \
            \ VALUES (?, ?)                  \
            \ RETURNING id                   "
            (aid, aid, DB.Binary (B.encode WM.emptyWorldModel))
     return Conversation
            { conversationID = cid
            }





-- | We can @PUT@ a 'REPLConversationUpdate' to
-- @/apps/:id/repl-conversations/:id@ to update a REPL conversation. Unlike a
-- normal conversation, a REPL conversation doesn't throw errors when the
-- input can't parse or solve. Instead it returns the errors as different
-- return values. Additionally, it doesn't log the errors. This is because a
-- REPL conversation is intended for diagnostic/debugging purposes.

apps_id_repl_conversations_id_put
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "repl-conversations" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] ConversationUpdateInfo
               :> Put '[JSON] REPLConversationChange)
apps_id_repl_conversations_id_put conn aid cid auth info =
  do ensureUserAuthorizedOnREPLConversation conn aid cid (Auth.userID auth)
     handleConvoUpdate info
  where
    handleConvoUpdate :: ConversationUpdateInfo
                      -> ExceptT ServantErr IO REPLConversationChange
    handleConvoUpdate (ConversationUpdateWorldModel newNextEnt newFcts) =
      do [DB.Only wmByteString] :: [DB.Only ByteString]
           <- liftIO $ DB.query
                conn
                "SELECT binary_rep FROM repl_conversations WHERE id = ?"
                (DB.Only cid)
         let wm = B.decode wmByteString :: WM.WorldModel
             newWorldModel =
               WM.WorldModel
               { WM.nextEntity = max (WM.nextEntity wm) newNextEnt
               , WM.facts = newFcts ++ WM.facts wm
               }
         _ <- liftIO $ DB.execute
                conn
                "UPDATE repl_conversations SET binary_rep = ? WHERE id = ?"
                (DB.Binary (B.encode newWorldModel), cid)
         return REPLConversationChange
                { replChange = Nothing
                }
    handleConvoUpdate ConversationUpdateResetWorldModel =
      do _ <- liftIO $ DB.execute
                conn
                "UPDATE repl_conversations SET binary_rep = ? WHERE id = ?"
                (DB.Binary (B.encode WM.emptyWorldModel), cid)
         return REPLConversationChange
                { replChange = Nothing
                }
    handleConvoUpdate (ConversationUpdateDiscourseMove mve) =
      do [DB.Only wmByteString] :: [DB.Only ByteString]
           <- liftIO $ DB.query
                conn
                "SELECT binary_rep FROM repl_conversations WHERE id = ?"
                (DB.Only cid)
         buildProducts :: [DB.Only ByteString]
           <- liftIO $ DB.query
                conn
                " SELECT packages.build_product         \
                \ FROM packages, apps                   \
                \ WHERE                                 \
                \   ( packages.id = ANY (apps.packages) \
                \     OR packages.is_prelude            \
                \   )                                   \
                \ AND apps.id = ?                       "
                (DB.Only aid)
         let LEExtract
               { extractEnvironment = env
               , extractWords = wds
               , extractRules = rles
               }
               = combineExtracts
                   [ B.decode exByteString
                   | DB.Only exByteString <- buildProducts
                   ]
             wm = B.decode wmByteString :: WM.WorldModel
             pinfo = IP.ProcessingInfo
                     { IP.lexicon = wds
                     , IP.grammarRules = rles
                     , IP.environment = env
                     , IP.worldModel = wm
                     }
         case IP.processInput pinfo mve of
           Left err -> case err of
             Nothing ->
               return $ REPLConversationError MiscError
             Just (C.UnknownWord w) ->
               return $ REPLConversationError (UnknownWord w)
             Just (C.IncompleteParse c) ->
               return $
                 REPLConversationError
                   (IncompleteParse (C.chartToBracketedSequences c))
           Right (newWm,fcts) ->
             do _ <- liftIO $ DB.execute
                       conn
                       " UPDATE repl_conversations \
                       \ SET binary_rep = ?        \
                       \ WHERE id = ?              "
                       (DB.Binary (B.encode newWm), cid)
                return REPLConversationChange
                       { replChange = Just (WMFacts newWm fcts)
                       }