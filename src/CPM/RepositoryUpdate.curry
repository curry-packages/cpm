------------------------------------------------------------------------------
--- This module implements operations to update and change the
--- package repository, i.e., the index of all packages known
--- to the package manager.
------------------------------------------------------------------------------

module CPM.RepositoryUpdate
  ( addPackageToRepository, updateRepository
  )
 where

import Directory
import FilePath
import System            ( system )

import CPM.Config        ( Config, packageInstallDir, packageIndexRepository
                         , repositoryDir )
import CPM.ErrorLogger
import CPM.Package
import CPM.FileUtil      ( copyDirectory, inDirectory, removeDirectoryComplete )
import CPM.Repository
import CPM.RepositoryCache.Init ( tryWriteRepositoryDB )
import CPM.RepositoryCache.Select ( addPackageToRepositoryCache
                                  , updatePackageInRepositoryCache )

------------------------------------------------------------------------------
--- Updates the package index from the central Git repository.
--- Cleans also the global package cache in order to support
--- downloading the newest versions.
updateRepository :: Config -> IO (ErrorLogger ())
updateRepository cfg = do
  cleanRepositoryCache cfg
  debugMessage $ "Deleting global package cache: '" ++
                 packageInstallDir cfg ++ "'"
  removeDirectoryComplete (packageInstallDir cfg)
  gitExists <- doesDirectoryExist $ (repositoryDir cfg) </> ".git"
  if gitExists 
    then do
      c <- inDirectory (repositoryDir cfg) $ execQuietCmd $ cleanPullCmd
      if c == 0
        then finishUpdate
        else failIO $ "Failed to update git repository, return code " ++ show c
    else do
      c <- inDirectory (repositoryDir cfg) $ execQuietCmd cloneCommand
      if c == 0
        then finishUpdate
        else failIO $ "Failed to update git repository, return code " ++ show c
 where
  cleanPullCmd q = "git clean -d -f && git reset " ++ q ++ " --hard && " ++
                   "git pull " ++ q ++ " origin master"

  cloneCommand q = unwords ["git clone", q, packageIndexRepository cfg, "."]

  finishUpdate = do
    setLastUpdate cfg
    cleanRepositoryCache cfg
    log Info "Successfully downloaded repository index"
    tryWriteRepositoryDB cfg

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
    let pkgName          = name pkg
        pkgVersion       = version pkg
        pkgIndexDir      = pkgName </> showVersion pkgVersion
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
    when cpdir $ copyDirectory pkgdir pkgInstallDir
    if exrepodir then updatePackageInRepositoryCache cfg pkg
                 else addPackageToRepositoryCache    cfg pkg

------------------------------------------------------------------------------
