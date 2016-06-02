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
import Data.Aeson
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Servant







data Authorization =
  Authorization { userID :: UserID, isAdmin :: Bool }
  deriving (Generic,Show)

instance ToJSON Authorization





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