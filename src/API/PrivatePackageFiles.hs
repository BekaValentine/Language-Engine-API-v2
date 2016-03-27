{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines how to manage private package files.

module API.PrivatePackageFiles where

import API.APITypes
import qualified API.Authorization as Auth

import Control.Monad.Except
import Control.Monad.IO.Class ()
import Data.Aeson
import GHC.Generics
import qualified Database.PostgreSQL.Simple as DB
import Servant







-- | A 'PrivatePackageFileSummary' consists of a file's ID and its name.

data PrivatePackageFileSummary
  = PrivatePackageFileSummary
    { fileIDSummary :: FileID
    , fileNameSummary :: String
    }
  deriving (Generic)

instance FromJSON PrivatePackageFileSummary
instance ToJSON PrivatePackageFileSummary





-- | A 'PrivatePackageFile' consists of a file's ID, it's name, and its source
-- code.

data PrivatePackageFile
  = PrivatePackageFile
    { fileID :: FileID
    , fileName :: String
    , fileSourceCode :: String
    }
  deriving (Generic)

instance FromJSON PrivatePackageFile
instance ToJSON PrivatePackageFile





-- | A 'PrivatePackageFileConfig' consists of a file's name, its containing
-- package, and its source code.

data PrivatePackageFileConfig
  = PrivatePackageFileConfig
    { fileNameConfig :: String
    , filePackageIDConfig :: PackageID
    , fileSourceCodeConfig :: String
    }
  deriving (Generic)

instance FromJSON PrivatePackageFileConfig
instance ToJSON PrivatePackageFileConfig





-- | A 'PrivatePackageFileUpdate' consists of an optional file name and an
-- optional source text.

data PrivatePackageFileUpdate
  = PrivatePackageFileUpdate
    { fileNameUpdate :: Maybe String
    , fileSourceCodeUpdate :: Maybe String
    }
  deriving (Generic)

instance FromJSON PrivatePackageFileUpdate
instance ToJSON PrivatePackageFileUpdate





-- | We can ensure that a user with a specified ID is authorized to access
-- a specified file by confirming that they are the owner of the package that
-- the file belongs to.

ensureUserAuthorized :: DB.Connection
                     -> FileID
                     -> UserID
                     -> ExceptT ServantErr IO ()
ensureUserAuthorized conn fid uid =
  do foundFiles :: [DB.Only FileID]
        <- liftIO $ DB.query
             conn
             " SELECT private_package_files.id                         \
             \ FROM private_packages, private_package_files            \
             \ WHERE private_package_files.id=?                        \
             \ AND private_package_files.package = private_packages.id \
             \ AND private_packages.owner=?                            "
             (fid, uid)
     when (null foundFiles)
          (throwError err401)





-- | We can ensure that a user with a specified ID is authorized to modify a
-- given package by confirming they're the owner of that package.

ensureUserAuthorizedOnPackage
  :: DB.Connection
  -> PackageID
  -> UserID
  -> ExceptT ServantErr IO ()
ensureUserAuthorizedOnPackage conn pid uid =
  do foundPkgs :: [DB.Only PackageID]
       <- liftIO $ DB.query
            conn
            "SELECT id FROM private_packages WHERE id = ? AND owner = ?"
            (pid,uid)
     when (null foundPkgs)
          (throwError err401)





-- | The private package file API consists of creating a file by @POST@ing a
-- 'PrivatePackageFileConfig' to @/private_package_files@, getting a file by
-- @GET@ing @/private_package_files/:id@, updating a file by @PUT@ing a
-- 'PrivatePackageFileUpdate' to @/private_package_files/:id@, and deleting a
-- file by @DELETE@ing @/private_package_files/:id@.

type PrivatePackageFilesAPI =
  
       "private_package_files"
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] PrivatePackageFileConfig
         :> Post '[JSON] PrivatePackageFileSummary
  
  :<|> "private_package_files" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] PrivatePackageFile
  
  :<|> "private_package_files" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] PrivatePackageFileUpdate
         :> Put '[JSON] ()
  
  :<|> "private_package_files" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()





privatePackageFilesServer :: DB.Connection -> Server PrivatePackageFilesAPI
privatePackageFilesServer conn =
       private_package_files_post conn
  :<|> private_package_files_id_get conn
  :<|> private_package_files_id_put conn
  :<|> private_package_files_id_delete conn





-- | We can create a new package file by @POST@ing a
-- 'PrivatePackageFileConfig' to @/private_package_files@, ensuring that the
-- user is authorized to add a file to the specified package, updating the DB,
-- then returning an appropriate summary.

private_package_files_post
  :: DB.Connection
  -> Server ("private_package_files"
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] PrivatePackageFileConfig
               :> Post '[JSON] PrivatePackageFileSummary)
private_package_files_post conn auth (PrivatePackageFileConfig nme pid src) =
  do ensureUserAuthorizedOnPackage conn pid (Auth.userID auth)
     fileCreateRows :: [DB.Only FileID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO private_package_files \
            \   (name,package,source_code)      \
            \ VALUES (?, ?, ?)                  \
            \ RETURNING id                      "
            (nme,pid,src)
     case fileCreateRows of
       [DB.Only fid] ->
         do _ <- liftIO $ DB.execute
                   conn
                   " UPDATE private_packages \
                   \ SET needs_build = true  \
                   \ WHERE id = ?            "
                   (DB.Only pid)
            return $ PrivatePackageFileSummary fid nme
       _ -> throwError err500





-- | We can get the info about a file by @GET@ing from
-- @/private_package_files/:id@, ensuring that the user is authorized to
-- access this file, querying the DB, and then returning the appropriate info.

private_package_files_id_get
  :: DB.Connection
  -> Server ("private_package_files" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Get '[JSON] PrivatePackageFile)
private_package_files_id_get conn fid auth =
  do ensureUserAuthorized conn fid (Auth.userID auth)
     foundFiles :: [(String,String)]
       <- liftIO $ DB.query
            conn
            "SELECT name,source_code FROM private_package_files WHERE id=?"
            (DB.Only fid)
     case foundFiles of
       [] -> throwError err404
       _:_:_ -> throwError err500
       [(nme,src)] ->
         return $ PrivatePackageFile fid nme src





-- | We can update the name and/or source code for a file by @PUT@ing a
-- 'PrivatePackageFileUpdate' to @/private_package_files/:id@, ensuring that
-- the user is authorized to update the file, and then making the necessary
-- updates to the DB.

private_package_files_id_put
  :: DB.Connection
  -> Server ("private_package_files" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] PrivatePackageFileUpdate
               :> Put '[JSON] ())
private_package_files_id_put conn fid auth update =
  do ensureUserAuthorized conn fid (Auth.userID auth)
     liftIO $ DB.withTransaction conn $ do
       updateName (fileNameUpdate update)
       updateSourceCode (fileSourceCodeUpdate update)
     return ()
  where
    updateName :: Maybe String -> IO ()
    updateName Nothing = return ()
    updateName (Just nme) =
      do _ <- DB.execute
                conn
                "UPDATE private_package_files SET name = ? WHERE id = ?"
                (nme,fid)
         return ()
    
    updateSourceCode :: Maybe String -> IO ()
    updateSourceCode Nothing = return ()
    updateSourceCode (Just src) =
      do [DB.Only pid] :: [DB.Only PackageID]
           <- DB.query
                conn
                " UPDATE private_package_files \
                \ SET source_code = ?          \
                \ WHERE id = ?                 \
                \ RETURNING package            "
                (src,fid)
         _ <- DB.execute
                conn
                "UPDATE private_packages SET needs_build = true WHERE id = ?"
                (DB.Only pid)
         return ()





-- | We can delete a file by @DELETE@ing @/private_package_files/:id@,
-- ensuring that the user is authorized to delete the file, and then updating
-- the DB.

private_package_files_id_delete
  :: DB.Connection
  -> Server ("private_package_files" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Delete '[JSON] ())
private_package_files_id_delete conn fid auth =
  do ensureUserAuthorized conn fid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM private_package_files WHERE id = ?"
            (DB.Only fid)
     return ()