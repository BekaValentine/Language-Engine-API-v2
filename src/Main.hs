{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the main server functionality of Language Engine.

module Main where

import API.Apps
import API.Authorization
import API.Login
import API.Packages
import API.Users

import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as DB
import Network.Mail.SMTP
import Network.Socket (HostName,PortNumber)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment
import System.IO

import Paths_Language_Engine_API_v2







-- | The API has a number of sub-APIs for various functionalities.

type LanguageEngineAPI =
       LoginAPI
  :<|> UsersAPI
  :<|> AppsAPI
  :<|> PackagesAPI





apiServer :: DB.Connection
          -> HostName
          -> PortNumber
          -> Address
          -> UserName
          -> Password
          -> Server LanguageEngineAPI
apiServer conn hn pn ea un pw
          =
       loginServer conn hn pn ea un pw
  :<|> usersServer conn
  :<|> appsServer conn
  :<|> packagesServer conn





type LanguageEngineSite = Raw





siteServer :: FilePath -> Server LanguageEngineSite
siteServer fp
  = serveDirectory (fp ++ "/Site")





-- | The whole LE server type consists of a site server and an api server.

type LanguageEngineServer =
       "api" :> LanguageEngineAPI
  :<|> "site" :> LanguageEngineSite





server :: DB.Connection
       -> FilePath 
       -> HostName
       -> PortNumber
       -> Address
       -> UserName
       -> Password
       -> Server LanguageEngineServer
server conn fp hn pn ea un pw
  =
       apiServer conn hn pn ea un pw
  :<|> siteServer fp





languageEngineServer :: Proxy LanguageEngineServer
languageEngineServer = Proxy




serverContext :: DB.Connection -> Context '[BasicAuthCheck Authorization]
serverContext conn = authCheck conn :. EmptyContext





-- basicAuthMain :: IO ()
-- basicAuthMain = run 8080 (serveWithContext basicAuthApi serverContext server)

main :: IO ()
main = do
  fp <- getDataFileName ""
  putStrLn $ "data file location: " ++ fp
  hSetBuffering stdout LineBuffering
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  url <- fmap BS.pack (getEnv "DATABASE_URL")
  conn <- DB.connectPostgreSQL url
  {-
  conn <- DB.connectPostgreSQL
            "host=localhost user=language_engine_admin \
            \password=password dbname=language_engine"
  --}
  putStrLn $ "Running server on port " ++ show port ++ "..."
  let supportHostname = "smtpout.secureserver.net"
      supportPortNumber = 80
      supportEmail = Address (Just "Language Engine Support")
                             "support@languagengine.co"
      supportUsername = "support@languagengine.co"
      supportPassword = "7W6C#ZFBNrnZGw4GT45h"
  run port
    $ simpleCors
    $ serveWithContext languageEngineServer (serverContext conn)
    $ server conn fp supportHostname supportPortNumber supportEmail supportUsername supportPassword