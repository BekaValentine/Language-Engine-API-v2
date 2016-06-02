{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the login API.

module API.Login where

import qualified API.Authorization as Auth
import API.APITypes
import Control.Monad.Except
import Control.Monad.IO.Class ()
import Crypto.BCrypt
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Text (pack)
import Data.Text.Lazy (pack)
import qualified Database.PostgreSQL.Simple as DB
import GHC.Generics
import Network.Mail.SMTP
import Network.Socket (HostName,PortNumber)
import Servant
import System.Random







-- | 'LoginInformation' consists of a username and password.

data LoginInformation
  = LoginInformation
    { loginUsername :: String
    , loginPassword :: String
    }
  deriving (Generic)

instance FromJSON LoginInformation





-- | A 'UsernameRecovery' consists of just an email to look up in the DB.

data UsernameRecovery
  = UsernameRecovery
    { emailAddress :: String
    }
  deriving (Generic)

instance FromJSON UsernameRecovery





-- | A 'PasswordResetRequest' consists of only a username to reset.

data PasswordResetRequest
  = PasswordResetRequest
    { username :: String
    }
  deriving (Generic)

instance FromJSON PasswordResetRequest





-- | A 'PasswordResetConfirmation' consists of a reset code and a new password
-- to change to.

data PasswordResetConfirmation
  = PasswordResetConfirmation
    { resetUsername :: String
    , resetCode :: String
    , resetNewPassword :: String
    }
  deriving (Generic)

instance FromJSON PasswordResetConfirmation





-- | The logic API consists of four actions. The first is confirming a login
-- is valid by @GET@ing from @/login/confirm@. The second is requesting the
-- username associated with an email address is sent to the email address, by
-- @POST@ing a 'UsernameRecovery' to @/login/username-recovery@. Third is
-- requesting a password reset by @POST@ing a 'PasswordResetRequest' to
-- @/login/password-reset-request@. Lastly, confirming a password reset is
-- done by @POST@ing a 'PasswordResetConfirm' to
-- @/login/password-reset-confirmation@

type LoginAPI =
  
       "login" :> "confirmation"
         :> ReqBody '[JSON] LoginInformation
         :> Post '[JSON] Auth.Authorization
  
  :<|> "login" :> "username-recovery"
         :> ReqBody '[JSON] UsernameRecovery
         :> Post '[JSON] ()
  
  :<|> "login" :> "password-reset-request"
         :> ReqBody '[JSON] PasswordResetRequest
         :> Post '[JSON] ()
  
  :<|> "login" :> "password-reset-confirmation"
         :> ReqBody '[JSON] PasswordResetConfirmation
         :> Post '[JSON] ()

{-
:<|> "login" :> "username-recovery" :> ReqBody '[JSON] UsernameRecovery :> Post '[JSON] ()
           :<|> "login" :> "password-reset-request" :> ReqBody '[JSON] PasswordResetRequest :> Post '[JSON] ()
           :<|> "login" :> "password-reset-confirm" :> ReqBody '[JSON] PasswordResetConfirm :> Post '[JSON] ()
-}




loginServer :: DB.Connection
            -> HostName
            -> PortNumber
            -> Address
            -> UserName
            -> Password
            -> Server LoginAPI
loginServer conn hn pn ea un pw =
       login_confirmation_post conn
  :<|> login_username_recovery_post conn hn pn ea un pw
  :<|> login_password_reset_request_post conn hn pn ea un pw
  :<|> login_password_reset_confirmation_post conn





-- | Login confirmation requires a trivial database lookup

login_confirmation_post
  :: DB.Connection
  -> Server ("login" :> "confirm"
               :> ReqBody '[JSON] LoginInformation
               :> Post '[JSON] Auth.Authorization)
login_confirmation_post conn (LoginInformation un pw) =
  do rs :: [(UserID,String,Bool)]
       <- liftIO $ DB.query
            conn
            "SELECT id,password,is_admin FROM users WHERE username=?"
            (DB.Only un)
     case rs of
       [(uid,hashedpw,isadmin)] ->
         if validatePassword (BS.pack hashedpw) (BS.pack pw)
         then return $ Auth.Authorization uid isadmin
         else throwError (err412 { errReasonPhrase = "Incorrect Password" })
       _ -> throwError (err412 { errReasonPhrase = "No Such User" })





-- | Username recovery involves emailing the user their usernames as found in
-- the database.

login_username_recovery_post
  :: DB.Connection
  -> HostName
  -> PortNumber
  -> Address
  -> UserName
  -> Password
  -> Server ("login" :> "username-recovery"
               :> ReqBody '[JSON] UsernameRecovery
               :> Post '[JSON] ())
login_username_recovery_post conn hn pn ea un pw (UsernameRecovery email) =
  do uns :: [DB.Only String]
       <- liftIO $ DB.query
            conn
            "SELECT username FROM users WHERE email_address = ?"
            (DB.Only email)
     case uns of
       [] -> throwError (err412 { errReasonPhrase = "Email Not Found" })
       _ ->
         do let (usernameWord,copula) =
                  if null (tail uns)
                  then ("username","is")
                  else ("usernames","are")
                body = "Your " ++ usernameWord ++ " for Language Engine " ++
                         copula ++ ": " ++ unwords [ user
                                                   | DB.Only user <- uns
                                                   ]
                mail = simpleMail
                         ea -- from
                         [Address Nothing (Data.Text.pack email)] -- to
                         [] -- cc
                         [] -- bcc
                         "Username Recovery" -- subject
                         [plainTextPart (Data.Text.Lazy.pack body)] -- body
            liftIO $ sendMailWithLogin' hn pn un pw mail





-- | Requesting a password reset involves emailing the user a newly generated
-- random unique string.

login_password_reset_request_post
  :: DB.Connection
  -> HostName
  -> PortNumber
  -> Address
  -> UserName
  -> Password
  -> Server ("login" :> "password-reset-request"
               :> ReqBody '[JSON] PasswordResetRequest
               :> Post '[JSON] ())
login_password_reset_request_post
  conn hn pn ea un pw (PasswordResetRequest user) =
    
  do uids :: [(UserID,String)]
       <- liftIO $ DB.query
            conn
            "SELECT id, email_address FROM users WHERE username = ?"
            (DB.Only user)
     case uids of
       [] -> throwError (err412 { errReasonPhrase = "User Not Found" })
       _:_:_ -> throwError err500
       [(uid,email)] ->
         do codeList <-
              liftIO $ sequence (replicate 10 (randomRIO (0,9) :: IO Int))
            let code = concat (map show codeList)
            insertionInfo :: [DB.Only UserID]
              <- liftIO $ DB.query
                   conn
                   " DELETE FROM password_reset WHERE user_id = ? ;   \
                   \ INSERT INTO password_reset (user_id, reset_code) \
                   \ VALUES (?,?) RETURNING user_id                   "
                   (uid, uid, code)
            case insertionInfo of
              [_] ->
                do let body = "Your password reset code is " ++ code ++ "."
                                ++ " All previous codes are no longer valid."
                       mail = simpleMail
                         ea -- from
                         [Address Nothing (Data.Text.pack email)] -- to
                         [] -- cc
                         [] -- bcc
                         "Password Reset" -- subject
                         [plainTextPart (Data.Text.Lazy.pack body)] -- body
                   liftIO $ sendMailWithLogin' hn pn un pw mail
              _ -> throwError err500





-- | Confirming a password reset involves updating the DB with a new PW when
-- the user supplies a valid reset code.

login_password_reset_confirmation_post
  :: DB.Connection
  -> Server ("login" :> "password-reset-confirmation"
               :> ReqBody '[JSON] PasswordResetConfirmation
               :> Post '[JSON] ())
login_password_reset_confirmation_post
  conn (PasswordResetConfirmation un code pw) =
  do uids :: [DB.Only UserID]
       <- liftIO $ DB.query
            conn
            " SELECT password_reset.user_id           \
            \ FROM password_reset, users              \
            \ WHERE password_reset.user_id = users.id \
            \ AND password_reset.reset_code = ?       \
            \ AND users.username = ?                  "
            (code,un)
     case uids of
       [] -> throwError (err412 { errReasonPhrase = "Reset Code Not Found" })
       _:_:_ -> throwError err500
       [DB.Only uid] ->
         do Just hpw
              <- liftIO $ hashPasswordUsingPolicy
                   fastBcryptHashingPolicy
                   (BS.pack pw)
            let hashedpw = BS.unpack hpw
            _ <- liftIO $ DB.execute
                   conn
                   " UPDATE users SET password = ? WHERE id = ? ; \
                   \ DELETE FROM password_reset                   \
                   \ WHERE user_id = ?                            "
                   (hashedpw, uid, uid)
            return ()



{-
do uids :: [DB.Only UserID]
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
-}