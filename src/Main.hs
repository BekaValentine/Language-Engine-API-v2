{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the main server functionality of Language Engine.

module Main where

import API.Apps
import API.Authorization
import API.Packages
import API.Users

import qualified Database.PostgreSQL.Simple as DB
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment
import System.IO







-- | The API has a number of sub-APIs for various functionalities.

type LanguageEngineAPI =
       UsersAPI
  :<|> AppsAPI
  :<|> PackagesAPI





apiServer :: DB.Connection
          -- -> HostName
          -- -> PortNumber
          -- -> EmailAddress
          -- -> UserName
          -- -> Password
          -> Server LanguageEngineAPI
apiServer conn -- hn pn ea un pw
          =
       
       usersServer conn
  :<|> appsServer conn
  :<|> packagesServer conn





-- | The whole LE server type consists of a site server and an api server.

type LanguageEngineServer =
  "api" :> LanguageEngineAPI
--  :<|> "site" :> LanguageEngineSite





server :: DB.Connection
       -- -> FilePath 
       -- -> HostName
       -- -> PortNumber
       -- -> EmailAddress
       -- -> UserName
       -- -> Password
       -> Server LanguageEngineServer
server conn -- fp hn pn ea un pw
  =
  apiServer conn --hn pn ea un pw
--  :<|> siteServer fp





languageEngineServer :: Proxy LanguageEngineServer
languageEngineServer = Proxy




serverContext :: DB.Connection -> Context '[BasicAuthCheck Authorization]
serverContext conn = authCheck conn :. EmptyContext





-- basicAuthMain :: IO ()
-- basicAuthMain = run 8080 (serveWithContext basicAuthApi serverContext server)

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
  conn <- DB.connectPostgreSQL
            "host=localhost user=language_engine_admin \
            \password=password dbname=language_engine"
  putStrLn $ "Running server on port " ++ show port ++ "..."
  {-
  let supportHostname = "smtp.mandrillapp.com"
      supportPortNumber = 587
      supportEmail = "support@languagengine.co"
      supportUsername = "support@languagengine.co"
      supportPassword = "1QKW7SgMhK4KdOVL-QdlKA"
      -}
  run port
    $ simpleCors
    $ serveWithContext languageEngineServer (serverContext conn)
    $ server conn --fp supportHostname supportPortNumber supportEmail supportUsername supportPassword