{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}







-- | This module defines the API for managine packages.

module API.Packages where

import qualified API.Authorization as Auth
import API.APITypes
import APIUtils.GrammarExtraction
import APIUtils.PackageBuilds

import Control.Monad.Except
import Control.Monad.IO.Class ()
import Data.Aeson
import qualified Data.Binary as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import GHC.Generics
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import Servant







-- | A 'PackageSummary' consists of an ID, a name, an owner ID, whether or not
-- the package uses prelude, whether or not it is a prelude package, and
-- whether or not it needs to be built.

data PackageSummary
  = PackageSummary
    { packageIDSummary :: PackageID
    , packageNameSummary :: String
    , packageDescriptionSummary :: String
    , packageIsOwnSummary :: Bool
    , packageUsesPreludeSummary :: Bool
    , packageIsPreludeSummary :: Bool
    , packageIsPublicSummary :: Bool
    , packageNeedsBuildSummary :: Bool
    , packageModuleNamesSummary :: [String]
    }
  deriving (Generic)

instance ToJSON PackageSummary





-- | A 'Package' consists of an ID, a name, an owner ID, whether or not the
-- package uses prelude, whether or not it is a prelude package, whether or
-- not it needs to be built, and a list of summaries of the package's files.
data Package
  = Package
    { packageID :: PackageID
    , packageName :: String
    , packageDescription :: String
    , packageIsOwn :: Bool
    , packageUsesPrelude :: Bool
    , packageIsPrelude :: Bool
    , packageIsPublic :: Bool
    , packageNeedsBuild :: Bool
    , fileSummaries :: [FileSummary]
    }
  deriving (Generic)

instance ToJSON Package





-- | A 'PackageConfig' consists of just a name.

data PackageConfig
  = PackageConfig
    { packageNameConfig :: String
    }
  deriving (Generic)

instance FromJSON PackageConfig





-- | A 'PackageUpdate' consists of an optional name update optional prelude
-- use update, and an optional is-prelude update.

data PackageUpdate
  = PackageUpdate
    { packageNameUpdate :: Maybe String
    , packageDescriptionUpdate :: Maybe String
    , packageUsesPreludeUpdate :: Maybe Bool
    , packageIsPreludeUpdate :: Maybe Bool
    , packageIsPublicUpdate :: Maybe Bool
    }
  deriving (Generic)

instance FromJSON PackageUpdate





-- | A 'FileSummary' consists of a file's id and its name.

data FileSummary
  = FileSummary
    { fileIDSummary :: FileID
    , fileNameSummary :: String
    , fileDescriptionSummary :: String
    }
  deriving (Generic)

instance ToJSON FileSummary





-- | A 'File' consists of a file's ID, it's name, and its source code.

data File
  = File
    { fileID :: FileID
    , fileName :: String
    , fileDescription :: String
    , fileSourceCode :: String
    }
  deriving (Generic)

instance ToJSON File





-- | A 'FileConfig' consists of a file's name, its containing package, and its
-- source code.

data FileConfig
  = FileConfig
    { fileNameConfig :: String
    }
  deriving (Generic)

instance FromJSON FileConfig





-- | A 'FileUpdate' consists of an optional file name and an optional source
-- code.

data FileUpdate
  = FileUpdate
    { fileNameUpdate :: Maybe String
    , fileDescriptionUpdate :: Maybe String
    , fileSourceCodeUpdate :: Maybe String
    }
  deriving (Generic)

instance FromJSON FileUpdate





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
             "SELECT id FROM packages WHERE id = ? AND owner = ?"
             (pid, uid)
     when (null foundPkgs)
          (throwError err401)





-- | We can ensure that a user is authorized to view a package by checking
-- that they own it or that it's public.

-- | We can ensure that a user with a specified ID is authorized to access
-- a specified package by confirming that they are the owner of the package.

ensureUserAuthorizedToView
  :: DB.Connection
  -> PackageID
  -> UserID
  -> ExceptT ServantErr IO ()
ensureUserAuthorizedToView conn pid uid =
  do foundPkgs :: [DB.Only PackageID]
        <- liftIO $ DB.query
             conn
             " SELECT id                           \
             \ FROM packages                       \
             \ WHERE id = ?                        \
             \ AND (owner = ? OR is_public = true) "
             (pid, uid)
     when (null foundPkgs)
          (throwError err401)





-- | We can ensure that a specified package contains a specified file by
-- looking in the DB.

ensureUserAuthorizedOnFile :: DB.Connection
                           -> PackageID
                           -> FileID
                           -> UserID
                           -> ExceptT ServantErr IO ()
ensureUserAuthorizedOnFile conn pid fid uid =
  do foundFiles :: [DB.Only FileID]
       <- liftIO $ DB.query
            conn
            " SELECT files.id                 \
            \ FROM files, packages            \
            \ WHERE files.id = ?              \
            \ AND files.package = packages.id \
            \ AND packages.id = ?             \
            \ AND packages.owner = ?          "
            (fid,pid,uid)
     when (null foundFiles)
          (throwError err404)





-- | We can ensure that a user is authorized to view a particular file by
-- checking that they own it's parent package or the package is public.

ensureUserAuthorizedOnFileToView
  :: DB.Connection
  -> PackageID
  -> FileID
  -> UserID
  -> ExceptT ServantErr IO ()
ensureUserAuthorizedOnFileToView conn pid fid uid =
  do foundFiles :: [DB.Only FileID]
       <- liftIO $ DB.query
            conn
            " SELECT files.id                                       \
            \ FROM files, packages                                  \
            \ WHERE files.id = ?                                    \
            \ AND files.package = packages.id                       \
            \ AND packages.id = ?                                   \
            \ AND (packages.owner = ? OR packages.is_public = true) "
            (fid,pid,uid)
     when (null foundFiles)
          (throwError err404)





-- | The API for packages consists of @GET@ing the end point @/packages@ to
-- list the packages the user has created, @POST@ing to a 'PackageConfig' to
-- @/packages@ to create a new package, @GET@ing a package's info from
-- @/packages/:id@, @PUT@ing a 'PackageUpdate' to @/packages/:id@, and
-- @DELETE@ing a package at @/packages/:id@.
--
-- Individual packages also have a sub-endpoint for builds. @PUT@ing to
-- @/packages/:id/build@ triggers a build action on the package, returning a
-- result.
--
-- Packages also have sub-endpoints for files. @POST@ing a 'FileConfig' to
-- @/packages/:id/files@ will create a file, @PUT@ing a 'FileUpdate' to
-- @/packages/:id/files/:id@ will modify a file, and @DELETE@ing
-- @/packages/:id/files/:id@ will delete it.

type PackagesAPI =
  
       "packages"
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] [PackageSummary]
  
  :<|> "packages"
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] PackageConfig
         :> Post '[JSON] PackageSummary
  
  :<|> "packages" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] Package
  
  :<|> "packages" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] PackageUpdate
         :> Put '[JSON] ()
  
  :<|> "packages" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()
  
  :<|> "packages" :> CaptureID :> "build"
         :> BasicAuth "le-realm" Auth.Authorization
         :> Put '[JSON] ()
  
  :<|> "packages" :> CaptureID :> "files"
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] FileConfig
         :> Post '[JSON] FileSummary
  
  :<|> "packages" :> CaptureID :> "files" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Get '[JSON] File
  
  :<|> "packages" :> CaptureID :> "files" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> ReqBody '[JSON] FileUpdate
         :> Put '[JSON] ()
  
  :<|> "packages" :> CaptureID :> "files" :> CaptureID
         :> BasicAuth "le-realm" Auth.Authorization
         :> Delete '[JSON] ()
         




packagesServer :: DB.Connection -> Server PackagesAPI
packagesServer conn =
       packages_get conn
  :<|> packages_post conn
  :<|> packages_id_get conn
  :<|> packages_id_put conn
  :<|> packages_id_delete conn
  :<|> packages_id_build_put conn
  :<|> packages_id_files_post conn
  :<|> packages_id_files_id_get conn
  :<|> packages_id_files_id_put conn
  :<|> packages_id_files_id_delete conn





-- | We can list the packages a user has by @GET@ing @/packages@, and then
-- querying the DB.

packages_get
  :: DB.Connection
  -> Server ("packages"
               :> BasicAuth "le-realm" Auth.Authorization
               :> Get '[JSON] [PackageSummary])
packages_get conn auth =
  do pkgs :: [(PackageID,String,String,UserID,Bool
              ,Bool,Bool,Bool,DB.PGArray String)]
       <- liftIO $ DB.query
            conn
            " SELECT                                             \
            \   id, name, description, owner, uses_prelude,      \
            \   is_prelude, is_public, needs_build, module_names \
            \ FROM packages                                      \
            \ WHERE owner = ?                                    \
            \ OR is_public = true                                "
            (DB.Only (Auth.userID auth))
     return [ PackageSummary
              { packageIDSummary = pid
              , packageNameSummary = nme
              , packageDescriptionSummary = desc
              , packageIsOwnSummary = owner == Auth.userID auth
              , packageUsesPreludeSummary = up
              , packageIsPreludeSummary = isp
              , packageIsPublicSummary = ispub
              , packageNeedsBuildSummary = nb
              , packageModuleNamesSummary = modNames
              }
            | (pid,nme,desc,owner,up,isp,ispub,nb,DB.PGArray modNames) <- pkgs
            ]
       




-- | We can add a new package for a user by @POST@ing a 'PackageConfig' to
-- @/packages@ and then inserting into the DB with the ID of the current user.
-- We then return a 'Package' with the returned package ID, the package name,
-- and the user ID.

packages_post
  :: DB.Connection
  -> Server ("packages"
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] PackageConfig
               :> Post '[JSON] PackageSummary)
packages_post conn auth (PackageConfig nme) =
  do packageCreateRows :: [DB.Only PackageID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO packages                      \
            \   (name,owner,build_product,module_names) \
            \ VALUES (?, ?, ?, '{}')                    \
            \ RETURNING id                              "
            ( nme
            , Auth.userID auth
            , DB.Binary (B.encode emptyExtract)
            )
     case packageCreateRows of
       [DB.Only pid] ->
         return PackageSummary
                { packageIDSummary = pid
                , packageNameSummary = nme
                , packageDescriptionSummary = ""
                , packageIsOwnSummary = True
                , packageUsesPreludeSummary = True
                , packageIsPreludeSummary = False
                , packageIsPublicSummary = False
                , packageNeedsBuildSummary = False
                , packageModuleNamesSummary = []
                }
       _ -> throwError err500





-- | We can get the info for a package by @GET@ing from @/packages/:id@ and
-- then querying the DB, returning an appropriate summary of the package.

packages_id_get
  :: DB.Connection
  -> Server ("packages" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Get '[JSON] Package)
packages_id_get conn pid auth =
  do ensureUserAuthorizedToView conn pid (Auth.userID auth)
     foundPkgs :: [(String,String,UserID,Bool,Bool,Bool,Bool)]
       <- liftIO $ DB.query
            conn
            " SELECT                      \
            \   name, description, owner, \
            \   uses_prelude, is_prelude, \
            \   is_public, needs_build    \
            \ FROM packages               \
            \ WHERE id = ?                "
            (DB.Only pid)
     case foundPkgs of
       [] -> throwError err404
       _:_:_ -> throwError err500
       [(nme,desc,owner,up,isp,ispub,nb)] ->
         do foundFiles :: [(FileID,String,String)]
              <- liftIO $ DB.query
                   conn
                   "SELECT id, name, description FROM files WHERE package = ?"
                   (DB.Only pid)
            return $ Package
                     { packageID = pid
                     , packageName = nme
                     , packageDescription = desc
                     , packageIsOwn = owner == Auth.userID auth
                     , packageUsesPrelude = up
                     , packageIsPrelude = isp
                     , packageIsPublic = ispub
                     , packageNeedsBuild = nb
                     , fileSummaries =
                       [ FileSummary fid fnme fdesc
                       | (fid,fnme,fdesc) <- foundFiles
                       ]
                     }





-- | We can update the name of a package by @PUT@ing a 'PackageUpdate' to
-- @/packages/:id@ and then updating the DB. We then return an appropriate
-- 'Package'.

packages_id_put
  :: DB.Connection
  -> Server ("packages" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] PackageUpdate
               :> Put '[JSON] ())
packages_id_put conn pid auth update =
  do ensureUserAuthorized conn pid (Auth.userID auth)
     unless (packageIsPreludeUpdate update == Nothing
              || Auth.isAdmin auth)
       $ throwError err401
     ensurePreludeSettingsValid       
     liftIO $ DB.withTransaction conn $ do
       updateName (packageNameUpdate update)
       updateDescription (packageDescriptionUpdate update)
       updateUsesPrelude (packageUsesPreludeUpdate update)
       updateIsPrelude (packageIsPreludeUpdate update)
       updateIsPublic (packageIsPublicUpdate update)
     return ()
  where
    ensurePreludeSettingsValid :: ExceptT ServantErr IO ()
    ensurePreludeSettingsValid =
      do [(oldup,oldisp)] :: [(Bool,Bool)]
           <- liftIO $ DB.query
                conn
                " SELECT uses_prelude, is_prelude \
                \ FROM packages                   \
                \ WHERE id = ?"
                (DB.Only pid)
         let newup = maybe oldup id (packageUsesPreludeUpdate update)
             newisp = maybe oldisp id (packageIsPreludeUpdate update)
         unless (not (newup && newisp))
           $ throwError (err412 { errReasonPhrase = "Circular Prelude Use" })
    
    updateName :: Maybe String -> IO ()
    updateName = mapM_ $ \nme ->
      do _ <- DB.execute
                conn
                "UPDATE packages SET name = ? WHERE id = ?"
                (nme, pid)
         return ()
    
    updateDescription :: Maybe String -> IO ()
    updateDescription = mapM_ $ \desc ->
      do _ <- DB.execute
                conn
                "UPDATE packages SET description = ? WHERE id = ?"
                (desc,pid)
         return ()
    
    updateUsesPrelude :: Maybe Bool -> IO ()
    updateUsesPrelude = mapM_ $ \up ->
      do _ <- DB.execute
                conn
                "UPDATE packages SET uses_prelude = ? WHERE id = ?"
                (up, pid)
         return ()
    
    updateIsPrelude :: Maybe Bool -> IO ()
    updateIsPrelude = mapM_ $ \isp ->
      do _ <- DB.execute
                conn
                "UPDATE packages SET is_prelude = ? WHERE id = ?"
                (isp, pid)
         return ()
    
    updateIsPublic :: Maybe Bool -> IO ()
    updateIsPublic = mapM_ $ \ispub ->
      do _ <- DB.execute
                conn
                "UPDATE packages SET is_public = ? WHERE id = ?"
                (ispub,pid)
         return ()





-- | We can delete a package by @DELETE@ing @/packages/:id@.

packages_id_delete
  :: DB.Connection
  -> Server ("packages" :> CaptureID
              :> BasicAuth "le-realm" Auth.Authorization
              :> Delete '[JSON] ())
packages_id_delete conn pid auth =
  do ensureUserAuthorized conn pid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM packages WHERE id = ?"
            (DB.Only pid)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM files WHERE package = ?"
            (DB.Only pid)
     _ <- liftIO $ DB.execute
            conn
            "UPDATE apps SET packages = array_remove(packages,?)"
            (DB.Only pid)
     return ()





-- | We can build a package by @POST@ing to @/packages/:id/build@.

packages_id_build_put
  :: DB.Connection
  -> Server ("packages" :> CaptureID :> "build"
               :> BasicAuth "le-realm" Auth.Authorization
               :> Put '[JSON] ())
packages_id_build_put conn pid auth =
  do ensureUserAuthorized conn pid (Auth.userID auth)
     merr <- liftIO $ buildPackage conn pid
     case merr of
       Nothing -> return ()
       Just err ->
         throwError (err412 { errBody = pack err })





-- | We can create a new package file by @POST@ing a 'FileConfig' to
-- @/packages/:id/files@, ensuring that the user is authorized to add a file
-- to the package, updating the DB, then returning an appropriate summary.

packages_id_files_post
  :: DB.Connection
  -> Server ("packages" :> CaptureID :> "files"
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] FileConfig
               :> Post '[JSON] FileSummary)
packages_id_files_post conn pid auth (FileConfig nme) =
  do ensureUserAuthorized conn pid (Auth.userID auth)
     fileCreateRows :: [DB.Only FileID]
       <- liftIO $ DB.query
            conn
            " INSERT INTO files \
            \   (name,package)  \
            \ VALUES (?, ?)     \
            \ RETURNING id      "
            (nme,pid)
     case fileCreateRows of
       [DB.Only fid] ->
         return FileSummary
                { fileIDSummary = fid
                , fileNameSummary = nme
                , fileDescriptionSummary = ""
                }
       _ -> throwError err500





-- | We can get the info about a file by @GET@ing from
-- @/packages/:id/files/:id@, ensuring that the user is authorized to access
-- this file, querying the DB, and then returning the appropriate info.

packages_id_files_id_get
  :: DB.Connection
  -> Server ("packages" :> CaptureID :> "files" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Get '[JSON] File)
packages_id_files_id_get conn pid fid auth =
  do ensureUserAuthorizedOnFileToView conn pid fid (Auth.userID auth)
     foundFiles :: [(String,String,String)]
       <- liftIO $ DB.query
            conn
            "SELECT name, description, source_code FROM files WHERE id = ?"
            (DB.Only fid)
     case foundFiles of
       [] -> throwError err404
       _:_:_ -> throwError err500
       [(nme,desc,src)] ->
         return File
                { fileID = fid
                , fileName = nme
                , fileDescription = desc
                , fileSourceCode = src
                }





-- | We can update the name and/or source code for a file by @PUT@ing a
-- 'FileUpdate' to @/packages/:id/files/:id@, ensuring that the user is
-- authorized to update the file, and then making the necessary updates to
-- the DB.

packages_id_files_id_put
  :: DB.Connection
  -> Server ("packages" :> CaptureID :> "files" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> ReqBody '[JSON] FileUpdate
               :> Put '[JSON] ())
packages_id_files_id_put conn pid fid auth update =
  do ensureUserAuthorizedOnFile conn pid fid (Auth.userID auth)
     liftIO $ DB.withTransaction conn $ do
       updateName (fileNameUpdate update)
       updateDescription (fileDescriptionUpdate update)
       updateSourceCode (fileSourceCodeUpdate update)
     return ()
  where
    updateName :: Maybe String -> IO ()
    updateName = mapM_ $ \nme ->
      do _ <- DB.execute
                conn
                "UPDATE files SET name = ? WHERE id = ?"
                (nme,fid)
         return ()
    
    updateDescription :: Maybe String -> IO ()
    updateDescription = mapM_ $ \desc ->
      do _ <- DB.execute
                conn
                "UPDATE files SET description = ? WHERE id = ?"
                (desc,fid)
         return ()
    
    updateSourceCode :: Maybe String -> IO ()
    updateSourceCode  = mapM_ $ \src ->
      do _ <- DB.execute
                conn
                "UPDATE files SET source_code = ? WHERE id = ?"
                (src,fid)
         _ <- DB.execute
                conn
                "UPDATE packages SET needs_build = true WHERE id = ?"
                (DB.Only pid)
         return ()





-- | We can build a package by collecting up everything necessary and
-- extracting. If it fails, we return an error string.

buildPackage :: DB.Connection -> PackageID -> IO (Maybe String)
buildPackage conn pid =
  do [DB.Only up] :: [DB.Only Bool]
       <- liftIO $ DB.query
            conn
            "SELECT uses_prelude FROM packages WHERE id = ?"
            (DB.Only pid)
     fileSrcs :: [(String,String)]
       <- liftIO $ DB.query
            conn
            "SELECT name, source_code FROM files WHERE package = ?"
            (DB.Only pid)
     preludes :: [DB.Only ByteString]
       <- if up
          then
            liftIO $ DB.query_
              conn
              " SELECT build_product    \
              \ FROM packages           \
              \ WHERE is_prelude = true \
              \ AND is_public = true    "
          else
            return []
     let prelude :: [PackageBuild]
         prelude = [ B.decode p | DB.Only p <- preludes ]
     putStrLn $
       "Building package with id " ++ (show pid) ++ " with preludes "
         ++ unwords (map (unwords . packageModuleNames) prelude)
     case extract prelude fileSrcs of
       Left err ->
         do putStrLn $ "Build failed: " ++ err
            return (Just err)
       Right (modNames,ex,pkg) ->
         do putStrLn "Build succeeded."
            _ <- liftIO $ DB.execute
                   conn
                   " UPDATE packages         \
                   \ SET build_extract = ?,  \
                   \     build_product = ?,  \
                   \     module_names = ?,   \
                   \     needs_build = false \
                   \ WHERE id = ?            "
                   ( DB.Binary (B.encode ex)
                   , DB.Binary (B.encode pkg)
                   , DB.PGArray modNames
                   , pid
                   )
            return Nothing





-- | We can delete a file by @DELETE@ing @/packages/:id/files/:id@, ensuring
-- that the user is authorized to delete the file, and then updating the DB.

packages_id_files_id_delete
  :: DB.Connection
  -> Server ("packages" :> CaptureID :> "files" :> CaptureID
               :> BasicAuth "le-realm" Auth.Authorization
               :> Delete '[JSON] ())
packages_id_files_id_delete conn pid fid auth =
  do ensureUserAuthorizedOnFile conn pid fid (Auth.userID auth)
     _ <- liftIO $ DB.execute
            conn
            "DELETE FROM files WHERE id = ?"
            (DB.Only fid)
     me <- liftIO $ do
       DB.begin conn
       me <- buildPackage conn pid
       case me of
         Nothing ->
           do DB.commit conn
              return me
         Just _ ->
           do DB.rollback conn
              return me
     case me of
       Nothing -> return ()
       Just err -> throwError (err412 { errBody = pack err })