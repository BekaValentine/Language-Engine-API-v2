{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the API for managing users.

module API.Users where

import API.APITypes
import qualified API.Authorization as Auth

import Control.Monad.Except
import Control.Monad.IO.Class ()
import Crypto.BCrypt
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Servant







-- | A 'User' has a user ID, a username, an email, and can be an admin.

data User
  = User
    { userID :: UserID
    , username :: String
    , emailAddress :: String
    , isAdmin :: Bool
    }
  deriving (Generic,Show)

instance ToJSON User





-- | A 'UserConfig' is the information required to create a new user.

data UserConfig
  = UserConfig
    { usernameConfig :: String
    , passwordConfig :: String
    , emailAddressConfig :: String
    }
  deriving (Generic,Show)

instance FromJSON UserConfig





-- | A 'UserUpdate' is information required to modify a use. We can modify
-- their password, email address, or admin status.

data UserUpdate
  = UserUpdate
    { passwordUpdate :: Maybe String
    , emailAddressUpdate :: Maybe String
    }
  deriving (Generic,Show)

instance FromJSON UserUpdate





-- | We can ensure that a user with a specified ID exists by querying the DB
-- and erroring whenever that user is missing.

ensureUserExists :: DB.Connection -> UserID -> ExceptT ServantErr IO ()
ensureUserExists conn uid
  = do res :: [DB.Only UserID]
         <- liftIO $ DB.query
              conn
              "SELECT id FROM users WHERE id=?"
              (DB.Only uid)
       unless (not (null res))
         $ throwError (err404 { errReasonPhrase = "User Not Found" })





-- | We can ensure that a user with a specified ID is authorized to access
-- a specified user by confirming that they are in fact the same user.

ensureUserAuthorized :: UserID -> UserID -> ExceptT ServantErr IO ()
ensureUserAuthorized uid1 uid2 =
  unless (uid1 == uid2) (throwError err401)





-- | The user API permits you to post new users to @/users@, get info about a
-- user from @/users/:id@, and update info at @/users/:id@. The latter two
-- require authorization.

type UsersAPI =

       "users" :> ReqBody '[JSON] UserConfig :> Post '[JSON] User

  :<|> "users" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] User

  :<|> "users" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] UserUpdate
         :> Put '[JSON] ()





usersServer :: DB.Connection -> Server UsersAPI
usersServer conn =
         users_post conn
    :<|> users_id_get conn
    :<|> users_id_put conn





-- | We can add a user with a given config by first ensuring that there is no
-- existing user with that username, hashing the PW, then inserting the info
-- into the database. If the insertion was successful, we return a 'User',
-- otherwise we throw an internal server error.

users_post :: DB.Connection
           -> Server ("users"
                        :> ReqBody '[JSON] UserConfig
                        :> Post '[JSON] User)
users_post conn (UserConfig un pw email) =
  do userExistsRows :: [DB.Only UserID]
       <- liftIO $ DB.query
            conn
            "SELECT id FROM users WHERE username=?"
            (DB.Only un)
     unless (null userExistsRows)
       $ throwError (err412 { errReasonPhrase = "Existing User" })
     Just hpw <- liftIO $ hashPasswordUsingPolicy
                   fastBcryptHashingPolicy
                   (BS.pack pw)
     let hashedpw = BS.unpack hpw
     userCreateRows :: [DB.Only UserID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO users                               \
            \   (username, password, is_admin, email_address) \
            \ VALUES (?, ?, FALSE, ?)                         \
            \ RETURNING id                                    "
            (un, hashedpw, email)
     case userCreateRows of
       [DB.Only uid] ->
         return $ User { userID = uid
                       , username = un
                       , isAdmin = False
                       , emailAddress = email
                       }
       _ -> throwError err500





-- | We can get info about a user with a particular ID by ensuring that the
-- user in question exists, that the user trying to access the specified user
-- is authorized to access this user, and then finally querying the DB to get
-- and return the 'User'.

users_id_get :: DB.Connection
             -> Server ("users" :> CaptureID
                          :> BasicAuth "le-realm" Auth.Authorization
                          :> Get '[JSON] User)
users_id_get conn uid auth =
  do ensureUserExists conn uid
     ensureUserAuthorized uid (Auth.userID auth)
     [(un,isad,email)] :: [(String,Bool,String)]
       <- liftIO $ DB.query
            conn
            "SELECT username, is_admin, email_address FROM users WHERE id=?"
            (DB.Only uid)
     return User { userID = uid
                 , username = un
                 , isAdmin = isad
                 , emailAddress = email
                 }





users_id_put :: DB.Connection
             -> Server ("users" :> CaptureID
                          :> BasicAuth "le-realm" Auth.Authorization
                          :> ReqBody '[JSON] UserUpdate
                          :> Put '[JSON] ())
users_id_put conn uid auth update =
  do ensureUserExists conn uid
     ensureUserAuthorized uid (Auth.userID auth)
     liftIO $ DB.withTransaction conn $ do
       updatePassword (passwordUpdate update)
       updateEmailAddress (emailAddressUpdate update)
     return ()
  where
    updatePassword :: Maybe String -> IO ()
    updatePassword Nothing = return ()
    updatePassword (Just newpw) =
      do Just hpw <- hashPasswordUsingPolicy
                       fastBcryptHashingPolicy
                       (BS.pack newpw)
         let hashedpw = BS.unpack hpw
         _ <- DB.execute
                conn
                "UPDATE users SET password=? WHERE id=?"
                (hashedpw, uid)
         return ()
    
    updateEmailAddress :: Maybe String -> IO ()
    updateEmailAddress Nothing = return ()
    updateEmailAddress (Just newea) =
      do _ <- DB.execute
                conn
                "UPDATE users SET email_address=? WHERE id=?"
                (newea, uid)
         return ()