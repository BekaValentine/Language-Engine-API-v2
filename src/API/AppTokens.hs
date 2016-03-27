{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the API for managing application tokens, which are
-- used to ensure that a requesting application is authorized to use an app.

module API.AppTokens where

import API.APITypes
import qualified API.Authorization as Auth

import Control.Monad
import Control.Monad.Except
import Data.Aeson
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Servant
import System.Random







-- | An 'AppToken' represents an app token. It consits of the token's ID in
-- the database, the token string itself, and the ID of the app that it
-- belongs to.

data AppToken
  = AppToken
    { tokenID :: AppTokenID
    , token :: String
    , appID :: AppID
    }
  deriving (Generic,Show)

instance FromJSON AppToken
instance ToJSON AppToken





-- | An 'AppTokenConfig' represents how we can configure a new app token, and
-- consists of simply the ID of the app that the token belongs to.

data AppTokenConfig
  = AppTokenConfig
    { appIDConfig :: AppID
    }
  deriving (Generic,Show)

instance FromJSON AppTokenConfig
instance ToJSON AppTokenConfig





-- | We can ensure that a user is authorized to modify information about an
-- app token by checking that the user is the owner of the app that the token
-- belongs to.

ensureUserAuthorized :: DB.Connection
                     -> AppTokenID
                     -> Auth.Authorization
                     -> ExceptT ServantErr IO ()
ensureUserAuthorized conn atid auth
 = do foundTokens :: [DB.Only AppTokenID]
        <- liftIO $ DB.query
             conn
             " SELECT app_tokens.id FROM app_tokens, apps      \
             \ WHERE app_tokens.id = ?                         \
             \ AND app_tokens.app = apps.id AND apps.owner = ? "
             (atid, Auth.userID auth)
      when (null foundTokens)
           (throwError err401)





-- | We can ensure that a user is authorized to modify information about an
-- app, such as its tokens, by checking that the user is the owner of the app.

ensureUserAuthorizedOnApp :: DB.Connection
                          -> AppID
                          -> UserID
                          -> ExceptT ServantErr IO ()
ensureUserAuthorizedOnApp conn aid uid
 = do foundApps :: [DB.Only AppID]
        <- liftIO $ DB.query
             conn
             "SELECT id FROM apps WHERE id=? AND owner=?"
             (aid, uid)
      when (null foundApps)
           (throwError err401)





-- | The app token API has two end points. We can create a new app token by
-- posting to @/app_tokens@, or we can delete an app token by deleting
-- @/app_tokens/:id@.

type AppTokensAPI =
  
       "app_tokens"
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] AppTokenConfig
         :> Post '[JSON] AppToken
  
  :<|> "app_tokens" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()




appTokensServer :: DB.Connection -> Server AppTokensAPI
appTokensServer conn =
       app_tokens_post conn
  :<|> app_tokens_id_delete conn





-- | We can make a new app token for an app with id @aid@ by posting to
-- @/app_tokens@. We first check that the user is authorized on the app, and
-- then generate a random string of alphanumeric (@[a-zA-Z0-9]@) symbols of
-- length 30. We then insert this into the DB for the app.

app_tokens_post :: DB.Connection
                -> Server ("app_tokens"
                             :> BasicAuth "le-realm" Auth.Authorization
                             :> ReqBody '[JSON] AppTokenConfig
                             :> Post '[JSON] AppToken)
app_tokens_post conn auth (AppTokenConfig aid) =
  do ensureUserAuthorizedOnApp conn aid (Auth.userID auth)
     let syms = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
     newTokIndexes <-
       liftIO $ sequence (replicate 30 (randomRIO (0,length syms-1)))
     let newTok = [ syms !! i | i <- newTokIndexes ]
     insertionInfo :: [DB.Only AppTokenID]
       <- liftIO $ DB.query
            conn
            "INSERT INTO app_tokens (token,app) VALUES (?,?) RETURNING id"
            (newTok,aid)
     case insertionInfo of
       [DB.Only tid]
         -> return $ AppToken
                     { tokenID = tid
                     , token = newTok
                     , appID = aid
                     }
       _ -> throwError err500





-- | We can delete an app token by deleting at @/app_tokens/:id@. We first
-- check that the user is authorized to modify the token, and then we run a
-- deletion operation removing the token from the DB.

app_tokens_id_delete :: DB.Connection
                     -> Server ("app_tokens" :> CaptureID
                                  :> BasicAuth "le-realm" Auth.Authorization
                                  :> Delete '[JSON] ())
app_tokens_id_delete conn atid auth =
  do ensureUserAuthorized conn atid auth
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM app_tokens WHERE id=?"
            (DB.Only atid)
     return ()