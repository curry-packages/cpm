------------------------------------------------------------------------------
--- This module implements operations to update and change the
--- package repository, i.e., the index of all packages known
--- to the package manager.
------------------------------------------------------------------------------

module CPM.Repository.Update
  ( addPackageToRepository, updateRepository
  )
 where

import Directory
import FilePath
import List              ( isSuffixOf )
import System            ( system )

import CPM.Config        ( Config, packageInstallDir, packageIndexURL
                         , repositoryDir )
import CPM.ErrorLogger
import CPM.Package
import CPM.Package.Helpers    ( cleanPackage )
import CPM.FileUtil           ( copyDirectory, inDirectory, quote
                              , recreateDirectory, removeDirectoryComplete )
import CPM.Repository
import CPM.Repository.CacheDB ( tryWriteRepositoryDB )
import CPM.Repository.Select  ( addPackageToRepositoryCache
                              , updatePackageInRepositoryCache )

------------------------------------------------------------------------------
--- Updates the package index from the central Git repository.
--- If the second argument is `True`, also the global package cache
--- is cleaned in order to support downloading the newest versions.
--- If the third argument is `True`, the global package index is downloaded
--- from the central repository.
--- If the fourth argument is `True`, also a CSV file containing the
--- database entries is written.
updateRepository :: Config -> Bool -> Bool -> Bool -> IO (ErrorLogger ())
updateRepository cfg cleancache download writecsv = do
  cleanRepositoryCache cfg
  when cleancache $ do
    debugMessage $ "Deleting global package cache: '" ++
                   packageInstallDir cfg ++ "'"
    removeDirectoryComplete (packageInstallDir cfg)
  debugMessage $ "Recreating package index: '" ++ repositoryDir cfg ++ "'"
  if download
    then do
      recreateDirectory (repositoryDir cfg)
      c <- inDirectory (repositoryDir cfg) downloadCommand
      if c == 0
        then finishUpdate
        else failIO $ "Failed to update package index, return code " ++ show c
    else tryWriteRepositoryDB cfg writecsv
 where
  downloadCommand
    | ".git" `isSuffixOf` piurl
    = execQuietCmd $ \q -> unwords ["git clone", q, quote piurl, "."]
    | ".tar" `isSuffixOf` piurl
    = do let tarfile = "XXX.tar"
         c1 <- showExecCmd $ unwords ["curl", "-s", "-o", tarfile, quote piurl]
         c2 <- showExecCmd $ unwords ["tar", "-xf", tarfile]
         removeFile tarfile
         return (c1+c2)
    | ".tar.gz" `isSuffixOf` piurl
    = do let tarfile = "XXX.tar.gz"
         c1 <- showExecCmd $ unwords ["curl", "-s", "-o", tarfile, quote piurl]
         c2 <- showExecCmd $ unwords ["tar", "-xzf", tarfile]
         removeFile tarfile
         return (c1+c2)
    | otherwise
    = do errorMessage $ "Unknown kind of package index URL: " ++ piurl
         return 1
   where piurl = packageIndexURL cfg

  finishUpdate = do
    setLastUpdate cfg
    cleanRepositoryCache cfg
    infoMessage "Successfully downloaded repository index"
    tryWriteRepositoryDB cfg writecsv

--- Sets the date of the last update by touching README.md.
setLastUpdate :: Config -> IO ()
setLastUpdate cfg =
  system (unwords ["touch", repositoryDir cfg </> "README.md"]) >> done

------------------------------------------------------------------------------
--- Adds a package stored in the given directory to the repository index.
--- If the argument `force` is true, overwrite an already existing package.
--- If the argument `cpdir` is true, copy also the complete directory
--- into the local package installation store.
addPackageToRepository :: Config -> String -> Bool -> Bool
                       -> IO (ErrorLogger ())
addPackageToRepository cfg pkgdir force cpdir = do
  dirExists <- doesDirectoryExist pkgdir
  if dirExists
    then loadPackageSpec pkgdir |>= \pkgSpec ->
         (copyPackage pkgSpec >> succeedIO ()) |>
         log Info ("Package in directory '" ++ pkgdir ++
                   "' installed into local repository")
    else log Critical ("Directory '" ++ pkgdir ++ "' does not exist.") |>
         succeedIO ()
 where
  copyPackage pkg = do
    let pkgIndexDir      = name pkg </> showVersion (version pkg)
        pkgRepositoryDir = repositoryDir cfg </> pkgIndexDir
        pkgInstallDir    = packageInstallDir cfg </> packageId pkg
    exrepodir <- doesDirectoryExist pkgRepositoryDir
    when (exrepodir && not force) $ error $
      "Package repository directory '" ++
      pkgRepositoryDir ++ "' already exists!\n"
    expkgdir <- doesDirectoryExist pkgInstallDir
    when expkgdir $
      if force then removeDirectoryComplete pkgInstallDir
               else error $ "Package installation directory '" ++
                            pkgInstallDir ++ "' already exists!\n"
    infoMessage $ "Create directory: " ++ pkgRepositoryDir
    createDirectoryIfMissing True pkgRepositoryDir
    copyFile (pkgdir </> "package.json") (pkgRepositoryDir </> "package.json")
    when cpdir $ do copyDirectory pkgdir pkgInstallDir
                    inDirectory pkgInstallDir (cleanPackage cfg Debug)
                    done
    if exrepodir then updatePackageInRepositoryCache cfg pkg
                 else addPackageToRepositoryCache    cfg pkg

------------------------------------------------------------------------------
