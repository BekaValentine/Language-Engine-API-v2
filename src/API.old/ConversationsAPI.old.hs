{-# LANGUAGE DataKinds,
             DeriveGeneric,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators
             #-}

module API.ConversationsAPI where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import qualified Data.Binary as B
import Data.ByteString.Lazy (ByteString)
import Data.Int
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Servant

import qualified API.Authorization as Auth
import qualified API.HTTPStatusCodes as Status
import qualified Core.External.EventDescription as ED
import qualified Core.External.InputProcessing as IP
import qualified Core.External.Module as M
import qualified Core.External.WorldModel as WM
import Core.Result
import qualified Core.Semantics.Substitution as Subst
import qualified Core.Semantics.Term as Term


{-
instance FromJSON Term.EntityReferent
instance ToJSON Term.EntityReferent

instance FromJSON Term.EventReferent
instance ToJSON Term.EventReferent
-}

instance FromJSON WM.EntityFact
instance ToJSON WM.EntityFact

instance FromJSON WM.EventFact
instance ToJSON WM.EventFact

instance FromJSON WM.WorldModel
instance ToJSON WM.WorldModel



type ConversationID = Int32
type AppID = Int32
type UserID = Int32

type CaptureID = Capture "id" Int32

data Conversation
  = Conversation
    { conversationID :: ConversationID
    , appID :: AppID
    , worldModel :: WM.WorldModel
    }
  deriving (Generic)

instance FromJSON Conversation
instance ToJSON Conversation

data ConversationConfig
  = ConversationConfig
    { appIDConfig :: AppID
    }
  deriving (Generic)

instance FromJSON ConversationConfig
instance ToJSON ConversationConfig

data EntityRepresentation
  = OldEntity Int | NewEntity Int
  deriving (Generic)

instance FromJSON EntityRepresentation
instance ToJSON EntityRepresentation

data EntityFactRepresentation
  = StringVal { entityRep :: EntityRepresentation, strVal :: String }
  deriving (Generic)

instance FromJSON EntityFactRepresentation
instance ToJSON EntityFactRepresentation

data EventRepresentation
  = OldEvent Int | NewEvent Int
  deriving (Generic)

instance FromJSON EventRepresentation
instance ToJSON EventRepresentation

data EventFactRepresentation
  = EventPredicate
    { eventPred :: String
    , eventPredEventRep :: EventRepresentation
    }
  | EventEntityRelation
    { eventEntityRel :: String
    , eventEntityRelEventRep :: EventRepresentation
    , eventEntityRelArg :: EntityRepresentation
    }
  | EventEventRelation
    { eventEventRel :: String
    , eventEventRelEventRep :: EventRepresentation
    , eventEventRelArg :: EventRepresentation
    }
  deriving (Generic)

instance FromJSON EventFactRepresentation
instance ToJSON EventFactRepresentation

data ConversationUpdate
  = ConversationUpdateWorldModel
    { newEntities :: [Int]
    , newEvents :: [Int]
    , newEntityFacts :: [EntityFactRepresentation]
    , newEventFacts :: [EventFactRepresentation]
    }
  | ConversationUpdateResetWorldModel
  | ConversationUpdateDiscourseMove
    { move :: String
    }
  deriving (Generic)

instance FromJSON ConversationUpdate
instance ToJSON ConversationUpdate

data ConversationChange
  = ConversationChange
    { newWorldModel :: WM.WorldModel
    , eventDescriptions :: Maybe [ED.EventDescription]
    }
  deriving (Generic)

instance FromJSON ConversationChange
instance ToJSON ConversationChange

buildWorldUpdate :: [Int] -> [Int] -> [EntityFactRepresentation] -> [EventFactRepresentation] -> WM.WorldUpdate ()
buildWorldUpdate newEnts newEvs newEntFactsRep newEvFactsRep
  = do newEntMap <- newEntMapUpdate
       newEvMap <- newEvMapUpdate
       mapM_ WM.assertEntity (convertNewEntityFacts newEntMap newEntFactsRep)
       mapM_ WM.assertEvent (convertNewEventFacts newEntMap newEvMap newEvFactsRep)
  
  where newEntMapUpdate :: WM.WorldUpdate (Map.Map Int Term.EntityReferent)
        newEntMapUpdate = fmap Map.fromList (go newEnts)
          where go [] = return []
                go (i:is) = do ent <- WM.newEntity
                               r <- go is
                               return ((i,ent) : r)
        newEvMapUpdate :: WM.WorldUpdate (Map.Map Int Term.EventReferent)
        newEvMapUpdate = fmap Map.fromList (go newEvs)
          where go [] = return []
                go (i:is) = do ev <- WM.newEvent
                               r <- go is
                               return ((i,ev) : r)
        
        convertEntityRep :: Map.Map Int Term.EntityReferent -> EntityRepresentation -> Maybe Term.EntityReferent
        convertEntityRep _    (OldEntity i) = Just (Term.EntityReferent i)
        convertEntityRep subs (NewEntity i) = Map.lookup i subs
        
        convertEventRep :: Map.Map Int Term.EventReferent -> EventRepresentation -> Maybe Term.EventReferent
        convertEventRep _    (OldEvent i) = Just (Term.EventReferent i)
        convertEventRep subs (NewEvent i) = Map.lookup i subs
        
        convertNewEntityFacts :: Map.Map Int Term.EntityReferent -> [EntityFactRepresentation] -> [WM.EntityFact]
        convertNewEntityFacts _ [] = []
        convertNewEntityFacts subs (StringVal entRep s : fs)
          = case convertEntityRep subs entRep of
              Just ent -> WM.StringVal ent s : convertNewEntityFacts subs fs
              _ -> convertNewEntityFacts subs fs
        
        convertNewEventFacts :: Map.Map Int Term.EntityReferent -> Map.Map Int Term.EventReferent -> [EventFactRepresentation] -> [WM.EventFact]
        convertNewEventFacts _ _ [] = []
        convertNewEventFacts entSubs evSubs (EventPredicate prd evRep : fs)
          = case convertEventRep evSubs evRep of
              Just ev -> WM.EventPredicate prd ev : convertNewEventFacts entSubs evSubs fs
              _ -> convertNewEventFacts entSubs evSubs fs
        convertNewEventFacts entSubs evSubs (EventEntityRelation rel evRep entRep : fs)
          = case (convertEventRep evSubs evRep, convertEntityRep entSubs entRep) of
              (Just ev, Just ent) -> WM.EventEntityRelation rel ev ent : convertNewEventFacts entSubs evSubs fs
              _ -> convertNewEventFacts entSubs evSubs fs
        convertNewEventFacts entSubs evSubs (EventEventRelation rel evRep evRep' : fs)
          = case (convertEventRep evSubs evRep, convertEventRep evSubs evRep') of
              (Just ev, Just ev') -> WM.EventEventRelation rel ev ev' : convertNewEventFacts entSubs evSubs fs
              _ -> convertNewEventFacts entSubs evSubs fs



type ConversationsAPI =
--          "conversations" :> Header "Authorization" Auth.AuthInfo :> Get '[JSON] [Conversation]
          "conversations" :> ReqBody '[JSON] ConversationConfig
                          :> Post '[JSON] Conversation
     :<|> "conversations" :> CaptureID
                          :> ReqBody '[JSON] ConversationUpdate
                          :> Put '[JSON] ConversationChange


{-
ensureUserAuthorized :: DB.Connection -> ConversationID -> Auth.Authorization -> EitherT ServantErr IO ()
ensureUserAuthorized conn cid auth
 = do foundTokens :: [DB.Only ConversationID]
        <- liftIO $ DB.query
             conn
             " SELECT conversations.id                                                 \
             \ FROM conversations,apps                                                 \
             \ WHERE conversations.id=? AND conversations.app=apps.id AND apps.owner=? "
             (cid, Auth.userID auth)
      when (null foundTokens)
           Status.unauthorized_
-}

ensureUserAuthorizedOnApp :: DB.Connection -> AppID -> Auth.Authorization -> EitherT ServantErr IO ()
ensureUserAuthorizedOnApp conn aid auth
 = do foundApps :: [DB.Only AppID]
        <- liftIO $ DB.query
             conn
             "SELECT id FROM apps WHERE id=? AND owner=?"
             (aid, Auth.userID auth)
      when (null foundApps)
           Status.unauthorized_



-- | List conversations
-- Possible errors:
--   User not logged in: 401
conversations_get :: DB.Connection
                  -> Server ("conversations"
                          :> Header "Authorization" Auth.AuthInfo
                          :> Get '[JSON] [Conversation])
conversations_get conn mauth
  = do auth <- Auth.getAuthorization conn mauth
       foundConversations :: [(ConversationID,AppID,ByteString)]
         <- liftIO $ DB.query
              conn
              " SELECT conversations.id,conversations.app,conversations.binary_rep \
              \ FROM conversations, apps                                           \
              \ WHERE conversations.app = apps.id AND apps.owner = ?               "
              (DB.Only (Auth.userID auth))
       return [ Conversation
                { conversationID = cid
                , appID = aid
                , worldModel = B.decode wmStr
                }
              | (cid,aid,wmStr) <- foundConversations
              ]



conversations_post :: DB.Connection
                   -> Server ("conversations"
                           :> ReqBody '[JSON] ConversationConfig
                           :> Post '[JSON] Conversation)
conversations_post conn (ConversationConfig aid)
  = do insertionInfo :: [DB.Only ConversationID]
         <- liftIO $ DB.query
              conn
              "INSERT INTO conversations (app,binary_rep) VALUES (?,?) RETURNING id"
              (aid, DB.Binary(B.encode WM.emptyWorldModel))
       case insertionInfo of
         [DB.Only cid] ->
           return $ Conversation
                    { conversationID = cid
                    , appID = aid
                    , worldModel = WM.emptyWorldModel
                    }
         _ -> Status.internalServerError_


{-
conversations_id_get :: DB.Connection
                     -> Server ("conversations"
                             :> CaptureID
                             :> Header "Authorization" Auth.AuthInfo
                             :> Get Conversation)
conversations_id_get conn cid mauth
  = do auth <- Auth.getAuthorization conn mauth
       --ensureUserAuthorized conn cid auth
       foundConversations :: [(AppID,ByteString)]
         <- liftIO $ DB.query
              conn
              "SELECT app, binary_rep FROM conversations WHERE id=?"
              (DB.Only cid)
       case foundConversations of
         [(aid,wmStr)] ->
           return $ Conversation
                    { conversationID = cid
                    , appID = aid
                    , worldModel = B.decode wmStr
                    }
         _ -> Status.internalServerError_
-}


conversations_id_put :: DB.Connection
                     -> Server ("conversations"
                             :> CaptureID
                             :> ReqBody '[JSON] ConversationUpdate
                             :> Put '[JSON] ConversationChange)
conversations_id_put conn cid update
  = do case update of
         ConversationUpdateWorldModel newEnts newEvs newEntFacts newEvFacts
           -> do pubModStrs :: [DB.Only ByteString]
                   <- liftIO $ DB.query
                        conn
                        " SELECT public_modules.binary_rep                 \
                        \ FROM public_modules, apps, conversations         \
                        \ WHERE conversations.id = ?                       \
                        \ AND conversations.app = apps.id                  \
                        \ AND public_modules.id = ANY(apps.public_modules) "
                        (DB.Only cid)
                 
                 privModStrs :: [DB.Only ByteString]
                   <- liftIO $ DB.query
                        conn
                        " SELECT private_modules.binary_rep                  \
                        \ FROM private_modules, apps, conversations          \
                        \ WHERE conversations.id = ?                         \
                        \ AND conversations.app = apps.id                    \
                        \ AND private_modules.id = ANY(apps.private_modules) "
                        (DB.Only cid)
                 
                 let primMap = Map.fromList . concat
                             $ [ M.primitives (B.decode pubModStr) | DB.Only pubModStr <- pubModStrs ]
                            ++ [ M.primitives (B.decode privModStr) | DB.Only privModStr <- privModStrs ]
                     evFactsCorrect :: Maybe String
                     evFactsCorrect = fmap head . sequence $ do
                       evFact <- newEvFacts
                       case evFact of
                         EventPredicate prd _
                           -> case Map.lookup prd primMap of
                                Just ty | Subst.alpha [] ty (Term.Event Term.~~> Term.Prop)
                                  -> return Nothing
                                _ -> return $ Just $ "Predicate " ++ prd ++ " is not an event predicate."
                         EventEntityRelation rel _ _
                           -> case Map.lookup rel primMap of
                                Just ty | Subst.alpha [] ty (Term.Event Term.~~> Term.Entity Term.~~> Term.Prop)
                                  -> return Nothing
                                _ -> return $ Just $ "Relation " ++ rel ++ " is not an event-entity relation."
                         EventEventRelation rel _ _
                           -> case Map.lookup rel primMap of
                                Just ty | Subst.alpha [] ty (Term.Event Term.~~> Term.Event Term.~~> Term.Prop)
                                  -> return Nothing
                                _ -> return $ Just $ "Relation " ++ rel ++ " is not an event-event relation."
                 
                 unless (Nothing == evFactsCorrect)
                      $ Status.preconditionFailed (fromJust evFactsCorrect)
                 
                 -- old stuff
                 [DB.Only wmStr] :: [DB.Only ByteString]
                   <- liftIO $ DB.query
                        conn
                        "SELECT binary_rep FROM conversations WHERE id=?"
                        (DB.Only cid)
                 let wm = B.decode wmStr
                     wmu = buildWorldUpdate newEnts newEvs newEntFacts newEvFacts
                     (_,wm') = WM.runWorldUpdate wmu wm
                 [DB.Only _] :: [DB.Only AppID]
                   <- liftIO $ DB.query
                        conn
                        "UPDATE conversations SET binary_rep = ? WHERE id=? RETURNING app"
                        (DB.Binary (B.encode wm'), cid)
                 return $ ConversationChange
                          { newWorldModel = wm'
                          , eventDescriptions = Nothing
                          }
         ConversationUpdateResetWorldModel
           -> do let wm' = WM.emptyWorldModel
                 [DB.Only _] :: [DB.Only AppID]
                   <- liftIO $ DB.query
                        conn
                        "UPDATE conversations SET binary_rep = ? WHERE id=? RETURNING app"
                        (DB.Binary (B.encode wm'), cid)
                 return $ ConversationChange
                          { newWorldModel = wm'
                          , eventDescriptions = Nothing
                          }
         ConversationUpdateDiscourseMove mve
           -> do pubModStrs :: [DB.Only ByteString]
                   <- liftIO $ DB.query
                        conn
                        " SELECT public_modules.binary_rep                 \
                        \ FROM public_modules, apps, conversations         \
                        \ WHERE conversations.id = ?                       \
                        \ AND conversations.app = apps.id                  \
                        \ AND public_modules.id = ANY(apps.public_modules) "
                        (DB.Only cid)
                 
                 privModStrs :: [DB.Only ByteString]
                   <- liftIO $ DB.query
                        conn
                        " SELECT private_modules.binary_rep                  \
                        \ FROM private_modules, apps, conversations          \
                        \ WHERE conversations.id = ?                         \
                        \ AND conversations.app = apps.id                    \
                        \ AND private_modules.id = ANY(apps.private_modules) "
                        (DB.Only cid)
                 
                 [DB.Only wmStr] :: [DB.Only ByteString]
                   <- liftIO $ DB.query
                        conn
                        "SELECT binary_rep FROM conversations WHERE id = ?"
                        (DB.Only cid)
                 
                 let mods = [ B.decode pubModStr :: M.Module | DB.Only pubModStr <- pubModStrs ]
                         ++ [ B.decode privModStr :: M.Module | DB.Only privModStr <- privModStrs ]
                 
                 case M.makeModuleSystem mods of
                   Core.Result.Error _ -> Status.internalServerError_
                   Core.Result.Success sys -> do
                     let sig = M.moduleSystemToSignature sys
                         lxn = M.moduleSystemToLexicon sys
                         wm = B.decode wmStr :: WM.WorldModel
                     
                     case IP.processInput sig lxn wm mve of
                       Core.Result.Error e -> Status.preconditionFailed (show e)
                       Core.Result.Success (wm',_,edStr) -> do
                         _ <- liftIO $ DB.execute
                                conn
                                "UPDATE conversations SET binary_rep = ? WHERE id = ?"
                                (DB.Binary (B.encode wm'), cid)
                         return $ ConversationChange
                                  { newWorldModel = wm'
                                  , eventDescriptions = Just edStr
                                  }



conversationsServer :: DB.Connection -> Server ConversationsAPI
conversationsServer conn =
--       conversations_get conn
       conversations_post conn
  -- :<|> conversations_id_get conn
  :<|> conversations_id_put conn