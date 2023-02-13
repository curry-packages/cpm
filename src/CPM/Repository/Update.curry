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
import Data.List         ( isSuffixOf )
import Control.Monad

import CPM.Config        ( Config, packageInstallDir, packageIndexURLs
                         , repositoryDir )
import CPM.ErrorLogger
import CPM.Executables        ( getCurlCmd )
import CPM.Package
import CPM.Package.Helpers    ( cleanPackage )
import CPM.FileUtil           ( copyDirectory, quote, recreateDirectory
                              , removeDirectoryComplete )
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
updateRepository cfg cleancache download usecache writecsv = do
  cleanRepositoryCache cfg
  when cleancache $ do
    logDebug $ "Deleting global package cache: '" ++
               packageInstallDir cfg ++ "'"
    liftIOEL $ removeDirectoryComplete $ packageInstallDir cfg
  logDebug $ "Recreating package index: '" ++ repositoryDir cfg ++ "'"
  if download
    then do
      liftIOEL $ recreateDirectory $ repositoryDir cfg
      c <- inDirectoryEL (repositoryDir cfg)
                         (tryDownload (packageIndexURLs cfg))
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
    = let qcmd q = unwords ["git clone", q, quote piurl, "."]
      in execQuietCmd (qcmd "-q") (qcmd "")
    | ".tar" `isSuffixOf` piurl
    = do let tarfile = "INDEX.tar"
         curlcmd <- getCurlCmd
         c1 <- showExecCmd $ unwords [curlcmd, "-o", tarfile, quote piurl]
         c2 <- showExecCmd $ unwords ["tar", "-xf", tarfile]
         liftIOEL $ removeFile tarfile
         return (c1 + c2)
    | ".tar.gz" `isSuffixOf` piurl
    = do let tarfile = "INDEX.tar.gz"
         curlcmd <- getCurlCmd
         c1 <- showExecCmd $ unwords [curlcmd, "-o", tarfile, quote piurl]
         c2 <- showExecCmd $ unwords ["tar", "-xzf", tarfile]
         liftIOEL $ removeFile tarfile
         return (c1 + c2)
    | otherwise
    = do logError $ "Unknown kind of package index URL: " ++ piurl
         return 1

  finishUpdate = do
    setLastUpdate cfg
    cleanRepositoryCache cfg
    logInfo "Successfully downloaded repository index"
    tryInstallRepositoryDB cfg usecache writecsv

--- Sets the date of the last update by touching README.md.
setLastUpdate :: Config -> ErrorLogger ()
setLastUpdate cfg = do
  showExecCmd $ unwords ["touch", repositoryDir cfg </> "README.md"]
  return ()

------------------------------------------------------------------------------
--- Adds a package stored in the given directory to the repository index.
--- If the argument `force` is true, overwrite an already existing package.
--- If the argument `cpdir` is true, copy also the complete directory
--- into the local package installation store.
addPackageToRepository :: Config -> String -> Bool -> Bool -> ErrorLogger ()
addPackageToRepository cfg pkgdir force cpdir = do
  dirExists <- liftIOEL $ doesDirectoryExist pkgdir
  if dirExists
    then do pkgSpec <- loadPackageSpec pkgdir
            copyPackage pkgSpec
            logInfo $ "Package in directory '" ++ pkgdir ++
                          "' installed into local repository"
    else logCritical $ "Directory '" ++ pkgdir ++ "' does not exist."
 where
  copyPackage pkg = do
    let pkgIndexDir      = name pkg </> showVersion (version pkg)
        pkgRepositoryDir = repositoryDir cfg </> pkgIndexDir
        pkgInstallDir    = packageInstallDir cfg </> packageId pkg
    exrepodir <- liftIOEL $ doesDirectoryExist pkgRepositoryDir
    when (exrepodir && not force) $ error $
      "Package repository directory '" ++
      pkgRepositoryDir ++ "' already exists!\n"
    expkgdir <- liftIOEL $ doesDirectoryExist pkgInstallDir
    when expkgdir $
      if force then liftIOEL $ removeDirectoryComplete pkgInstallDir
               else error $ "Package installation directory '" ++
                            pkgInstallDir ++ "' already exists!\n"
    logInfo $ "Create directory: " ++ pkgRepositoryDir
    liftIOEL $ do
      createDirectoryIfMissing True pkgRepositoryDir
      copyFile (pkgdir </> packageSpecFile)
               (pkgRepositoryDir </> packageSpecFile)
    when cpdir $ do
      liftIOEL $ copyDirectory pkgdir pkgInstallDir
      inDirectoryEL pkgInstallDir $ cleanPackage cfg Debug
    if exrepodir then updatePackageInRepositoryCache cfg pkg
                 else addPackageToRepositoryCache    cfg pkg

------------------------------------------------------------------------------
