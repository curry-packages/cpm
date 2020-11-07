------------------------------------------------------------------------------
--- This module implements operations to update and change the
--- package repository, i.e., the index of all packages known
--- to the package manager.
------------------------------------------------------------------------------

module CPM.Repository.Update
  ( addPackageToRepository, updateRepository
  )
 where

import System.Directory
import System.FilePath
import System.Process   ( system )
import Data.List        ( isSuffixOf )
import Control.Monad
import Prelude hiding (log)

import CPM.Config        ( Config, packageInstallDir, packageIndexURLs
                         , repositoryDir )
import CPM.ErrorLogger
import CPM.Package
import CPM.Package.Helpers    ( cleanPackage )
import CPM.FileUtil           ( copyDirectory, inDirectory, quote
                              , recreateDirectory, removeDirectoryComplete )
import CPM.Repository
import CPM.Repository.CacheDB ( tryInstallRepositoryDB )
import CPM.Repository.Select  ( addPackageToRepositoryCache
                              , updatePackageInRepositoryCache )

------------------------------------------------------------------------------
--- Updates the package index from the central Git repository.
--- If the second argument is `True`, also the global package cache
--- is cleaned in order to support downloading the newest versions.
--- If the third argument is `True`, the global package index is recreated
--- by downloading it from the central repository.
--- If the fourth argument is `True`, the package database is created
--- by reading the CSV file `REPOSITORY_CACHE.csv` downloaded from
--- the tar files URL, otherwise by reading all package specifications.
--- If the fifth argument is `True`, also a CSV file containing the
--- database entries is written.
updateRepository :: Config -> Bool -> Bool -> Bool -> Bool -> ErrorLogger ()
updateRepository cfg cleancache download usecache writecsv = toELM $ do
  cleanRepositoryCache cfg
  when cleancache $ do
    debugMessage $ "Deleting global package cache: '" ++
                   packageInstallDir cfg ++ "'"
    liftIOErrorLogger $ removeDirectoryComplete (packageInstallDir cfg)
  debugMessage $ "Recreating package index: '" ++ repositoryDir cfg ++ "'"
  if download
    then do
      liftIOErrorMessage $ recreateDirectory (repositoryDir cfg)
      c <- liftIOErrorMessage $ inDirectory (repositoryDir cfg) (tryDownload (packageIndexURLs cfg))
      if c == 0
        then finishUpdate
        else fail $ "Failed to update package index, return code " ++ show c
    else tryInstallRepositoryDB cfg usecache writecsv
 where
  tryDownload []         = return 1
  tryDownload (url:urls) = do
    c <- downloadCommand url
    if c == 0 then return 0
              else tryDownload urls

  downloadCommand piurl
    | ".git" `isSuffixOf` piurl
    = execQuietCmd $ \q -> unwords ["git clone", q, quote piurl, "."]
    | ".tar" `isSuffixOf` piurl
    = do let tarfile = "INDEX.tar"
         c1 <- showExecCmd $ unwords ["curl", "-s", "-o", tarfile, quote piurl]
         c2 <- showExecCmd $ unwords ["tar", "-xf", tarfile]
         liftIOErrorLogger $ removeFile tarfile
         return (c1+c2)
    | ".tar.gz" `isSuffixOf` piurl
    = do let tarfile = "INDEX.tar.gz"
         c1 <- showExecCmd $ unwords ["curl", "-s", "-o", tarfile, quote piurl]
         c2 <- showExecCmd $ unwords ["tar", "-xzf", tarfile]
         liftIOErrorLogger $ removeFile tarfile
         return (c1+c2)
    | otherwise
    = do errorMessage $ "Unknown kind of package index URL: " ++ piurl
         return 1

  finishUpdate = do
    liftIOErrorLogger $ setLastUpdate cfg
    cleanRepositoryCache cfg
    infoMessage "Successfully downloaded repository index"
    tryInstallRepositoryDB cfg usecache writecsv

--- Sets the date of the last update by touching README.md.
setLastUpdate :: Config -> IO ()
setLastUpdate cfg =
  system (unwords ["touch", repositoryDir cfg </> "README.md"]) >> return ()

------------------------------------------------------------------------------
--- Adds a package stored in the given directory to the repository index.
--- If the argument `force` is true, overwrite an already existing package.
--- If the argument `cpdir` is true, copy also the complete directory
--- into the local package installation store.
addPackageToRepository :: Config -> String -> Bool -> Bool -> ErrorLogger ()
addPackageToRepository cfg pkgdir force cpdir = do
  dirExists <- liftIOErrorLogger $ doesDirectoryExist pkgdir
  if dirExists
    then do pkgSpec <- loadPackageSpecELM pkgdir
            copyPackage pkgSpec
            infoMessage $ "Package in directory '" ++ pkgdir ++
                          "' installed into local repository"
    else criticalMessage $ "Directory '" ++ pkgdir ++ "' does not exist."
 where
  copyPackage pkg = do
    let pkgIndexDir      = name pkg </> showVersion (version pkg)
        pkgRepositoryDir = repositoryDir cfg </> pkgIndexDir
        pkgInstallDir    = packageInstallDir cfg </> packageId pkg
    exrepodir <- liftIOErrorLogger $ doesDirectoryExist pkgRepositoryDir
    when (exrepodir && not force) $ error $
      "Package repository directory '" ++
      pkgRepositoryDir ++ "' already exists!\n"
    expkgdir <- liftIOErrorLogger $ doesDirectoryExist pkgInstallDir
    when expkgdir $
      if force then liftIOErrorLogger $ removeDirectoryComplete pkgInstallDir
               else error $ "Package installation directory '" ++
                            pkgInstallDir ++ "' already exists!\n"
    infoMessage $ "Create directory: " ++ pkgRepositoryDir
    liftIOErrorLogger $ do $
      createDirectoryIfMissing True pkgRepositoryDir
      copyFile (pkgdir </> "package.json")
               (pkgRepositoryDir </> "package.json")
      when cpdir $ do
        copyDirectory pkgdir pkgInstallDir
        inDirectory pkgInstallDir $ runELM $ cleanPackage cfg Debug
    if exrepodir then updatePackageInRepositoryCache cfg pkg
                 else addPackageToRepositoryCache    cfg pkg

------------------------------------------------------------------------------
