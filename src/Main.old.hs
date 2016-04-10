{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, TypeFamilies, TypeOperators #-}

import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import qualified Database.PostgreSQL.Simple as DB
import Network.Socket.Internal (PortNumber)
import Network.Mail.SMTP (UserName,Password)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment
import System.IO

import API.UsersAPI
import API.AppsAPI
import API.AppTokensAPI
import API.ConversationsAPI
import API.LoginAPI

import Paths_Language_Engine_API



--type HostName = String
--type EmailAddress = Text

type LanguageEngineAPI
  =    UsersAPI
  :<|> AppsAPI
  :<|> AppTokensAPI
  :<|> ConversationsAPI
  :<|> LoginAPI

apiServer :: DB.Connection -> HostName -> PortNumber -> EmailAddress -> UserName -> Password -> Server LanguageEngineAPI
apiServer conn supportHostname supportPortNumber supportEmail supportUsername supportPassword
    =    usersServer conn
    :<|> appsServer conn
    :<|> appTokensServer conn
    :<|> conversationsServer conn
    :<|> loginServer conn supportHostname supportPortNumber supportEmail supportUsername supportPassword



type LanguageEngineSite = Raw

siteServer :: FilePath -> Server LanguageEngineSite
siteServer fp
  = serveDirectory (fp ++ "/Site")



type LanguageEngineServer
  =    "site" :> LanguageEngineSite
  :<|> "api" :> LanguageEngineAPI

server :: DB.Connection -> FilePath -> HostName -> PortNumber -> EmailAddress -> UserName -> Password -> Server LanguageEngineServer
server conn fp hn pn ea un pw
  =    siteServer fp
  :<|> apiServer conn hn pn ea un pw

languageEngineServer :: Proxy LanguageEngineServer
languageEngineServer = Proxy



main :: IO ()
main = do
  --fp <- getDataFileName ""
  let fp = "./" -- for development only. on heroku, use above
  putStrLn $ "data file location: " ++ fp
  hSetBuffering stdout LineBuffering
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  --url <- fmap BS.pack (getEnv "DATABASE_URL")
  --conn <- DB.connectPostgreSQL url
  conn <- DB.connectPostgreSQL "host=localhost user=language_engine_admin password=password dbname=language_engine"
  putStrLn $ "Running server on port " ++ show port ++ "..."
  let supportHostname = "smtp.mandrillapp.com"
      supportPortNumber = 587
      supportEmail = "support@languagengine.co"
      supportUsername = "support@languagengine.co"
      supportPassword = "1QKW7SgMhK4KdOVL-QdlKA"
  run port $ simpleCors $ serve languageEngineServer $
    server conn fp supportHostname supportPortNumber supportEmail supportUsername supportPassword