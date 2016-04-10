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
import APIUtils.EntityBuilding
import APIUtils.GrammarExtraction
import APIUtils.InputProcessing
import APIUtils.WorldModel

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

instance FromJSON AppSummary
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
    }
  deriving (Generic)

instance FromJSON App
instance ToJSON App





-- | An 'AppConfig' consists of just the name of the app.

data AppConfig
  = AppConfig
    { appNameConfig :: String
    }
  deriving (Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig





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
instance ToJSON AppUpdate





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
    , packageNeedsBuildSummary :: Bool
    }
  deriving (Generic)

instance FromJSON PackageSummary
instance ToJSON PackageSummary





-- | A 'Token' consists of the token ID, and the token string.

data Token
  = Token
    { tokenID :: TokenID
    , tokenString :: String
    }
  deriving (Generic)

instance FromJSON Token
instance ToJSON Token





-- | A 'Conversation' consists of a conversation ID and an initial world
-- model.

data Conversation
  = Conversation
    { conversationID :: ConversationID
    }
  deriving (Generic)

instance FromJSON Conversation
instance ToJSON Conversation





-- | A 'ConversationUpdate' can be either a world model update, which puts
-- new entities and facts into the world mode, a world model reset, which
-- empties out the world model, and a discourse move, which consists of a
-- natural language input to process.

data ConversationUpdate
  = ConversationUpdateWorldModel
    { newNextEntity :: Int
    , newFacts :: [EntityDescription]
    }
  | ConversationUpdateResetWorldModel
  | ConversationUpdateDiscourseMove
    { move :: String
    }
  deriving (Generic)

instance FromJSON ConversationUpdate
instance ToJSON ConversationUpdate





-- | A 'ConversationChange' consists of an optional set of entity descriptions
-- that represent the possible response to a discourse move (if that's the
-- kind of update that caused the change).

data ConversationChange
  = ConversationChange
    { entityDescriptions :: Maybe [EntityDescription]
    }
  deriving (Generic)

instance FromJSON ConversationChange
instance ToJSON ConversationChange





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





-- | We can ensure that a user is authorized on a conversation by finding the
-- conversation in the DB with the specified ID and application such that the
-- user is the owner of the application and the convo belongs to the app.

ensureUserAuthorizedOnConversation
  :: DB.Connection
  -> AppID
  -> ConversationID
  -> UserID
  -> ExceptT ServantErr IO ()
ensureUserAuthorizedOnConversation conn aid cid uid =
  do foundConvos :: [DB.Only ConversationID]
       <- liftIO $ DB.query
            conn
            " SELECT conversations.id        \
            \ FROM conversations, apps       \
            \ WHERE conversations.id = ?     \
            \ AND conversations.app = app.id \
            \ AND app.id = ?                 \
            \ AND app.owner = ?              "
            (cid,aid,uid)
     when (null foundConvos)
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
         :> BasicAuth "le-realm" Auth.Authorization
         :> Post '[JSON] Conversation
  
  :<|> "apps" :> CaptureID :> "conversations" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] ConversationUpdate
         :> Put '[JSON] ConversationChange





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
     foundPkgs :: [(PackageID,String,String,Bool,Bool,Bool,Bool)]
       <- liftIO $ DB.query
            conn
            " SELECT                                \
            \   id, name, description, uses_prelude \
            \   is_prelude, is_public, needs_build  \
            \ FROM packages                         \
            \ WHERE id IN ?                         "
            (DB.Only (DB.In (DB.fromPGArray pkgs)))
     foundToks :: [(TokenID,String)]
       <- liftIO $ DB.query
            conn
            "SELECT id, token FROM tokens WHERE app = ?"
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
                  , packageNeedsBuildSummary = nb
                  }
                | (pid,pnme,pdesc,up,isp,ispub,nb) <- foundPkgs
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
                "UPDATE apps SET packages = ? WHERE id = ?"
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
              :> BasicAuth "le-realm" Auth.Authorization
              :> Post '[JSON] Conversation)
apps_id_conversations_post conn aid auth =
  do ensureUserAuthorized conn aid (Auth.userID auth)
     [DB.Only cid] :: [DB.Only ConversationID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO conversations \
            \   (app, binary_rep)       \
            \ VALUES (?, ?)             "
            (aid, B.encode emptyWorldModel)
     return Conversation
            { conversationID = cid
            }





-- | We can @PUT@ a 'ConversationUpdate' to @/apps/:id/conversations/:id@ to
-- update a conversation.

apps_id_conversations_id_put
  :: DB.Connection
  -> Server ("apps" :> CaptureID :> "conversations" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] ConversationUpdate
               :> Put '[JSON] ConversationChange)
apps_id_conversations_id_put conn aid cid auth update =
  do ensureUserAuthorizedOnConversation conn aid cid (Auth.userID auth)
     handleConvoUpdate update
  where
    handleConvoUpdate :: ConversationUpdate
                      -> ExceptT ServantErr IO ConversationChange
    handleConvoUpdate (ConversationUpdateWorldModel newNextEnt newFcts) =
      do [DB.Only wmByteString] :: [DB.Only ByteString]
           <- liftIO $ DB.query
                conn
                "SELECT binary_rep FROM conversations WHERE id = ?"
                (DB.Only cid)
         let wm = B.decode wmByteString :: WorldModel
             newFactTerms = map entityDescriptionToTerm newFcts
             newWorldModel =
               WorldModel
               { nextEntity = max (nextEntity wm) newNextEnt
               , facts = newFactTerms ++ facts wm
               }
         _ <- liftIO $ DB.execute
                conn
                "UPDATE conversations SET binary_rep = ? WHERE id = ?"
                (B.encode newWorldModel, cid)
         return ConversationChange
                { entityDescriptions = Nothing
                }
    handleConvoUpdate ConversationUpdateResetWorldModel =
      do _ <- liftIO $ DB.execute
                conn
                "UPDATE conversations SET binary_rep = ? WHERE id = ?"
                (B.encode emptyWorldModel, cid)
         return ConversationChange
                { entityDescriptions = Nothing
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
                " SELECT packages.build_product     \
                \ FROM packages, app                \
                \ WHERE packages.id IN app.packages \
                \ AND app.id = ?                    "
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
             wm = B.decode wmByteString :: WorldModel
             pinfo = ProcessingInfo
                     { lexicon = wds
                     , grammarRules = rles
                     , environment = env
                     , worldModel = wm
                     }
         case processInput pinfo mve of
           Nothing -> throwError err412
           Just (newWm,eds) ->
             do _ <- liftIO $ DB.execute
                       conn
                       "UPDATE conversations SET binary_rep = ? WHERE id = ?"
                       (B.encode newWm, cid)
                return ConversationChange
                       { entityDescriptions = Just eds
                       }