{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the API for managine private packages.

module API.PrivatePackages where

import qualified API.Authorization as Auth
import API.APITypes

import Control.Monad.Except
import Control.Monad.IO.Class ()
import Data.Aeson
import GHC.Generics
import qualified Database.PostgreSQL.Simple as DB
import Servant







-- | A 'PrivatePackageSummary' consists of an ID, a name, and an owner ID.

data PrivatePackageSummary
  = PrivatePackageSummary
    { packageIDSummary :: PackageID
    , packageNameSummary :: String
    , packageOwnerIDSummary :: UserID
    }
  deriving (Generic)

instance FromJSON PrivatePackageSummary
instance ToJSON PrivatePackageSummary





-- | A 'PrivatePackage' consists of an ID, a name, an owner ID, and a list of
-- file summaries.

data PrivatePackage
  = PrivatePackage
    { packageID :: PackageID
    , packageName :: String
    , packageOwnerID :: UserID
    , fileSummaries :: [PrivatePackageFileSummary]
    }
  deriving (Generic)

instance FromJSON PrivatePackage
instance ToJSON PrivatePackage





-- | A 'PrivatePackageFileSummary' consists of a file's id and its name.

data PrivatePackageFileSummary
  = PrivatePackageFileSummary
    { fileIDSummary :: FileID
    , fileNameSummary :: String
    }
  deriving (Generic)

instance FromJSON PrivatePackageFileSummary
instance ToJSON PrivatePackageFileSummary





-- | A 'PrivatePackageConfig' consists of just a name.

data PrivatePackageConfig
  = PrivatePackageConfig
    { packageNameConfig :: String
    }
  deriving (Generic)

instance FromJSON PrivatePackageConfig
instance ToJSON PrivatePackageConfig





-- | A 'PrivatePackageUpdate' consists of just a name update.

data PrivatePackageUpdate
  = PrivatePackageUpdate
    { packageNameUpdate :: String
    }
  deriving (Generic)

instance FromJSON PrivatePackageUpdate
instance ToJSON PrivatePackageUpdate





-- | We can ensure that a user with a specified ID is authorized to access
-- a specified package by confirming that they are the owner of the package.

ensureUserAuthorized :: DB.Connection
                     -> PackageID
                     -> UserID
                     -> ExceptT ServantErr IO ()
ensureUserAuthorized conn pid uid =
  do foundPkgs :: [DB.Only PackageID]
        <- liftIO $ DB.query
             conn
             "SELECT id FROM private_packages WHERE id=? AND owner=?"
             (pid, uid)
     when (null foundPkgs)
          (throwError err401)





-- | The API for packages consists of @GET@ing the end point
-- @/private_packages@ to list the packages the user has created, @POST@ing to
-- a 'PrivatePackageConfig' to @/private_packages@ to create a new package,
-- @GET@ing a package's info from @/private_packages/:id@, @PUT@ing a
-- 'PrivatePackageUpdate' to @/private_packages/:id@, and @DELETE@ing a
-- package at @/private_packages/:id@.

type PrivatePackagesAPI =
  
       "private_packages"
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] [PrivatePackageSummary]
  
  :<|> "private_packages"
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] PrivatePackageConfig
         :> Post '[JSON] PrivatePackageSummary
  
  :<|> "private_packages" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] PrivatePackage
  
  :<|> "private_packages" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] PrivatePackageUpdate
         :> Put '[JSON] ()
  
  :<|> "private_packages" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()





privatePackagesServer :: DB.Connection -> Server PrivatePackagesAPI
privatePackagesServer conn =
       private_packages_get conn
  :<|> private_packages_post conn
  :<|> private_packages_id_get conn
  :<|> private_packages_id_put conn
  :<|> private_packages_id_delete conn





-- | We can list the packages a user has by @GET@ing @/private_packages@, and
-- then querying the DB.

private_packages_get :: DB.Connection
                     -> Server ("private_packages"
                                  :> BasicAuth "le-realm" Auth.Authorization
                                  :> Get '[JSON] [PrivatePackageSummary])
private_packages_get conn auth =
  do pkgs :: [(PackageID,String)]
       <- liftIO $ DB.query
            conn
            "SELECT id,name FROM private_packages WHERE owner=?"
            (DB.Only (Auth.userID auth))
     return [ PrivatePackageSummary pid nme (Auth.userID auth)
            | (pid,nme) <- pkgs
            ]
       





-- | We can add a new package for a user by @POST@ing a 'PrivatePackageConfig'
-- to @/private_packages@ and then inserting into the DB with the ID of the
-- current user. We then return a 'PrivatePackage' with the returned package
-- ID, the package name, and the user ID.

private_packages_post :: DB.Connection
                      -> Server ("private_packages"
                                   :> BasicAuth "le-realm" Auth.Authorization
                                   :> ReqBody '[JSON] PrivatePackageConfig
                                   :> Post '[JSON] PrivatePackageSummary)
private_packages_post conn auth (PrivatePackageConfig nme) =
  do packageCreateRows :: [DB.Only PackageID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO private_packages \
            \   (name,owner)               \
            \ VALUES (?, ?)                \
            \ RETURNING id                 "
            (nme, Auth.userID auth)
     case packageCreateRows of
       [DB.Only pid] ->
         return $ PrivatePackageSummary pid nme (Auth.userID auth)
       _ -> throwError err500





-- | We can get the info for a package by @GET@ing from
-- @/private_packages/:id@ and then querying the DB, returning an appropriate
-- summary of the package.

private_packages_id_get
  :: DB.Connection
  -> Server ("private_packages" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Get '[JSON] PrivatePackage)
private_packages_id_get conn pid auth =
  do ensureUserAuthorized conn pid (Auth.userID auth)
     foundPkgs :: [DB.Only String]
       <- liftIO $ DB.query
            conn
            "SELECT name FROM private_packages WHERE id = ?"
            (DB.Only pid)
     case foundPkgs of
       [] -> throwError err404
       _:_:_ -> throwError err500
       [DB.Only nme] ->
         do foundFiles :: [(FileID,String)]
              <- liftIO $ DB.query
                   conn
                   " SELECT id, name            \
                   \ FROM private_package_files \
                   \ WHERE package = ?          "
                   (DB.Only pid)
            return $ PrivatePackage
                       pid
                       nme
                       (Auth.userID auth)
                       [ PrivatePackageFileSummary fid fnme
                       | (fid,fnme) <- foundFiles
                       ]


-- | We can update the name of a package by @PUT@ing a 'PrivatePackageUpdate'
-- to @/private_packages/:id@ and then updating the DB. We then return an
-- appropriate 'PrivatePackage'.

private_packages_id_put
  :: DB.Connection
  -> Server ("private_packages" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] PrivatePackageUpdate
               :> Put '[JSON] ())
private_packages_id_put conn pid auth (PrivatePackageUpdate nme) =
  do ensureUserAuthorized conn pid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "UPDATE private_packages SET name = ? WHERE id = ?"
            (nme, pid)
     return ()





-- | We can delete a package by @DELETE@ing @/private_packages/:id@.

private_packages_id_delete
  :: DB.Connection
  -> Server ("private_packages" :> CaptureID
              :> BasicAuth "le-realm" Auth.Authorization
              :> Delete '[JSON] ())
private_packages_id_delete conn pid auth =
  do ensureUserAuthorized conn pid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM private_packages WHERE id = ?"
            (DB.Only pid)
     return ()