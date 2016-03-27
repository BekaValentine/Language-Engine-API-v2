{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}







-- | This module defines some common authorization tools.

module API.Authorization where

import API.APITypes

import Crypto.BCrypt
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Servant







data Authorization =
  Authorization { userID :: UserID, isAdmin :: Bool }
  deriving (Generic,Show)





authCheck :: DB.Connection -> BasicAuthCheck Authorization
authCheck conn = BasicAuthCheck check
  where
    check :: BasicAuthData -> IO (BasicAuthResult Authorization)
    check (BasicAuthData username password) =
      do rs :: [(UserID,String,Bool)]
           <- liftIO $ DB.query
                conn
                "SELECT id,password,is_admin FROM users WHERE username=?"
                (DB.Only username)
         case rs of
           [(uid,hashedpw,isadmin)] ->
             if validatePassword (BS.pack hashedpw) password
             then return (Authorized (Authorization uid isadmin))
             else return BadPassword
           _ -> return NoSuchUser







{-


{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module API.Authorization where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Crypto.BCrypt
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.Text
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Servant
import Text.ParserCombinators.Parsec

import qualified API.HTTPStatusCodes as Status

data AuthInfo = AuthInfo { username :: String, password :: String }
  deriving (Show)

instance FromText AuthInfo where
  fromText t = case parse authParser "" (unpack t) of
                 Left _     -> Nothing
                 Right auth -> Just auth
    where authParser :: GenParser Char st AuthInfo
          authParser = do _ <- many space
                          _ <- string "LEAUTH"
                          _ <- many1 space
                          _ <- string "username="
                          un <- quotedString
                          _ <- char ','
                          _ <- string "password="
                          pw <- quotedString
                          return $ AuthInfo un pw
                      
          
          quotedString :: GenParser Char st String
          quotedString = do _ <- char '\''
                            s <- manyTill (try (char '\\' >> anyChar) <|> noneOf "\\")
                                          (try (char '\''))
                            return s

type UserID = Int32

data Authorization =
  Authorization { userID :: UserID, isAdmin :: Bool }
  deriving (Generic,Show)

getAuthorization :: DB.Connection
                 -> Maybe AuthInfo
                 -> EitherT ServantErr IO Authorization
getAuthorization conn mauth
  = case mauth of
      Nothing -> Status.unauthorized "missing_credentials"
      Just (AuthInfo un pw) -> do
        rs :: [(UserID,String,Bool)]
          <- liftIO $ DB.query
               conn
               "SELECT id,password,is_admin FROM users WHERE username=?"
               (DB.Only un)
        case rs of
          [(uid,hashedpw,isadmin)] -> do
            if validatePassword (BS.pack hashedpw) (BS.pack pw)
            then return (Authorization uid isadmin)
            else Status.unauthorized "incorrect_password"
          _ -> Status.unauthorized "user_not_found"

getEmail :: DB.Connection
         -> String
         -> EitherT ServantErr IO String
getEmail conn un
  = do memail :: [DB.Only String]
         <- liftIO $ DB.query
              conn
              "SELECT email FROM users WHERE username=?"
              (DB.Only un)
       case memail of
         [DB.Only em] -> return em
         _            -> Status.preconditionFailed "invalid_username"
         
         
-}