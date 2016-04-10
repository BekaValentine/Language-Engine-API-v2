{-# LANGUAGE DataKinds,
             DeriveGeneric,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators
             #-}

module API.LoginAPI where

-- import Control.Monad
import Control.Monad.IO.Class
import Crypto.BCrypt
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.Text (Text,pack)
import Data.Text.Lazy (pack)
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Network.Socket.Internal (PortNumber)
import Network.Mail.SMTP
import Servant
import System.Random

import qualified API.Authorization as Auth
import qualified API.HTTPStatusCodes as Status



type UserID = Int32
type HostName = String
type EmailAddress = Text

data LoginInfo
  = LoginInfo
    { userID :: UserID
    , isAdmin :: Bool
    }
  deriving (Generic)

instance FromJSON LoginInfo
instance ToJSON LoginInfo

data UsernameRecovery
  = UsernameRecovery
    { emailAddress :: String
    }
  deriving (Generic)

instance FromJSON UsernameRecovery
instance ToJSON UsernameRecovery

data PasswordResetRequest
  = PasswordResetRequest
    { username :: String
    }
  deriving (Generic)

instance FromJSON PasswordResetRequest
instance ToJSON PasswordResetRequest

data PasswordResetConfirm
  = PasswordResetConfirm
    { resetCode :: String
    , newPassword :: String
    }
  deriving (Generic)

instance FromJSON PasswordResetConfirm
instance ToJSON PasswordResetConfirm



type LoginAPI = "login" :> "confirm" :> Header "Authorization" Auth.AuthInfo :> Get '[JSON] LoginInfo
           :<|> "login" :> "username-recovery" :> ReqBody '[JSON] UsernameRecovery :> Post '[JSON] ()
           :<|> "login" :> "password-reset-request" :> ReqBody '[JSON] PasswordResetRequest :> Post '[JSON] ()
           :<|> "login" :> "password-reset-confirm" :> ReqBody '[JSON] PasswordResetConfirm :> Post '[JSON] ()



login_confirm_get :: DB.Connection
                  -> Server ("login"
                          :> "confirm"
                          :> Header "Authorization" Auth.AuthInfo
                          :> Get '[JSON] LoginInfo)
login_confirm_get conn mauth
  = do auth <- Auth.getAuthorization conn mauth
       return LoginInfo { userID = Auth.userID auth
                        , isAdmin = Auth.isAdmin auth
                        }



login_usernameRecovery_post :: DB.Connection -> HostName -> PortNumber -> EmailAddress -> UserName -> Password
                            -> Server ("login"
                                    :> "username-recovery"
                                    :> ReqBody '[JSON] UsernameRecovery
                                    :> Post '[JSON] ())
login_usernameRecovery_post conn hostname port supportEmail hostusername hostpassword (UsernameRecovery email)
  = do uns :: [DB.Only String]
         <- liftIO $ DB.query
              conn
              "SELECT username FROM users WHERE email_address = ?"
              (DB.Only email)
       case uns of
         [] -> Status.preconditionFailed "email_not_found"
         [DB.Only un] -> liftIO $ sendMailWithLogin'
                           hostname
                           port
                           hostusername
                           hostpassword
                           (simpleMail
                              (Address { addressName = Nothing, addressEmail = supportEmail }) -- from
                              [Address { addressName = Nothing, addressEmail = Data.Text.pack email}] -- to
                              [] -- cc
                              [] -- bcc
                              "Username Recovery" -- subject
                              [plainTextPart . Data.Text.Lazy.pack
                                $ "Your username for Language Engine is\n\n    " ++ un]) -- body of email
         _ -> liftIO $ sendMailWithLogin'
                hostname
                port
                hostusername
                hostpassword
                (simpleMail
                   (Address { addressName = Nothing, addressEmail = supportEmail }) -- from
                   [Address { addressName = Nothing, addressEmail = Data.Text.pack email}] -- to
                   [] -- cc
                   [] -- bcc
                   "Username Recovery" -- subject
                   [plainTextPart . Data.Text.Lazy.pack
                     $ "Your usernames for Language Engine are\n\n"
                    ++ unlines [ "    " ++ un | DB.Only un <- uns ]
                   ]) -- body of email



login_passwordResetRequest_post
  :: DB.Connection -> HostName -> PortNumber -> EmailAddress -> UserName -> Password
  -> Server ("login"
          :> "password-reset-request"
          :> ReqBody '[JSON] PasswordResetRequest
          :> Post '[JSON] ())
login_passwordResetRequest_post conn hostname port supportEmail hostusername hostpassword (PasswordResetRequest un)
  = do uids :: [(UserID,String)]
         <- liftIO $ DB.query
              conn
              "SELECT id,email_address FROM users WHERE username = ?"
              (DB.Only un)
       case uids of
         [(uid,email)] -> do
           codeList <- liftIO $ sequence (replicate 10 (randomRIO (0,9) :: IO Int))
           let code = concat (map show codeList)
           insertionInfo :: [DB.Only UserID]
             <- liftIO $ DB.query
                  conn
                  " DELETE FROM password_reset WHERE user_id = ? ;                                  \
                  \ INSERT INTO password_reset (user_id, reset_code) VALUES (?,?) RETURNING user_id "
                  (uid,uid, code)
           case insertionInfo of
             [_] -> liftIO $ sendMailWithLogin'
                      hostname
                      port
                      hostusername
                      hostpassword
                      (simpleMail
                         (Address { addressName = Nothing, addressEmail = supportEmail }) -- from
                         [Address { addressName = Nothing, addressEmail = Data.Text.pack email}] -- to
                         [] -- cc
                         [] -- bcc
                         "Password Reset" -- subject
                         [plainTextPart . Data.Text.Lazy.pack
                           $ "Your password reset code is " ++ code ++ ". Any previous codes are no longer valid."
                         ]) -- body of email
             _ -> Status.internalServerError_
         _ -> Status.preconditionFailed "user_not_found"



login_passwordResetConfirm_post
  :: DB.Connection -> Server ("login"
                           :> "password-reset-request"
                           :> ReqBody '[JSON] PasswordResetConfirm
                           :> Post '[JSON] ())
login_passwordResetConfirm_post conn (PasswordResetConfirm code newpw)
  = do uids :: [DB.Only UserID]
         <- liftIO $ DB.query
              conn
              "SELECT user_id FROM password_reset WHERE reset_code = ?"
              (DB.Only code)
       case uids of
         [DB.Only uid] -> do
           Just hpw <- liftIO $ hashPasswordUsingPolicy
                         fastBcryptHashingPolicy
                         (BS.pack newpw)
           let hashedpw = BS.unpack hpw
           _ <- liftIO $ DB.execute
                  conn
                  " UPDATE users SET password=? WHERE id=? ;       \
                  \ DELETE FROM password_reset WHERE reset_code = ?"
                  (hashedpw, uid, code)
           return ()
         _ -> Status.preconditionFailed "reset_code_not_found"



loginServer :: DB.Connection -> HostName -> PortNumber -> EmailAddress -> UserName -> Password -> Server LoginAPI
loginServer conn hostname port supportEmail hostusername hostpassword
  =    login_confirm_get conn
  :<|> login_usernameRecovery_post conn hostname port supportEmail hostusername hostpassword
  :<|> login_passwordResetRequest_post conn hostname port supportEmail hostusername hostpassword
  :<|> login_passwordResetConfirm_post conn