--------------------------------------------------------------------------------
--- Contains functions that operate on a package copy. And some functions that
--- don't quite fit anywhere else.
--------------------------------------------------------------------------------

module CPM.PackageCopy
  ( resolveDependenciesForPackageCopy
  , resolveAndCopyDependencies, resolveAndCopyDependenciesForPackage
  , resolveDependencies
  , upgradeAllPackages
  , upgradeSinglePackage
  , tryFindLocalPackageSpec
  , linkToLocalCache
  , acquireAndInstallPackageWithDependencies
  , installLocalDependencies
  , renderPackageInfo
  ) where

import Debug
import Directory (doesFileExist, getAbsolutePath, createDirectoryIfMissing
                 , doesDirectoryExist, getTemporaryDirectory
                 , getCurrentDirectory, setCurrentDirectory, createDirectory
                 , removeDirectory, getDirectoryContents, copyFile)
import FilePath ((</>), takeExtension, takeBaseName, joinPath, takeDirectory)
import AbstractCurry.Types (CurryProg)
import List (intercalate, splitOn)
import Maybe (mapMaybe, fromJust)
import System (system)
import Pretty hiding ((</>))

import CPM.AbstractCurry
import CPM.Config (Config, packageInstallDir)
import CPM.Repository (Repository, allPackages)
import qualified CPM.LookupSet as LS
import CPM.ErrorLogger
import qualified CPM.PackageCache.Global as GC
import qualified CPM.PackageCache.Runtime as RuntimeCache
import qualified CPM.PackageCache.Local as LocalCache
import CPM.Package ( Package (..)
                   , readPackageSpec, packageId, readVersion, Version 
                   , showVersion, PackageSource (..), showDependency
                   , showCompilerDependency
                   , Dependency, GitRevision (..), PackageExecutable (..)
                   , PackageTest (..)
                   , packageIdEq, loadPackageSpec)
import CPM.Resolution
import CPM.FileUtil (copyDirectory, recreateDirectory)

--- Resolves dependencies for a package copy.
resolveDependenciesForPackageCopy :: Config -> Package -> Repository 
                                  -> GC.GlobalCache -> String 
                                  -> IO (ErrorLogger ResolutionResult)
resolveDependenciesForPackageCopy cfg pkg repo gc dir = 
  lookupSetForPackageCopy cfg pkg repo gc dir |>= \lookupSet ->
  resolveDependenciesFromLookupSet pkg lookupSet

--- Calculates the lookup set needed for dependency resolution on a package
--- copy.
lookupSetForPackageCopy :: Config -> Package -> Repository -> GC.GlobalCache 
                        -> String -> IO (ErrorLogger LS.LookupSet)
lookupSetForPackageCopy cfg _ repo gc dir =
  LocalCache.allPackages dir |>=
  \localPkgs -> do
    diffInLC <- mapIO filterGCLinked localPkgs
    let lsLC = LS.addPackages lsGC localPkgs LS.FromLocalCache in
      mapEL logSymlinkedPackage (mapMaybe id diffInLC) |>
      succeedIO lsLC
 where
  logSymlinkedPackage p = log Debug $ "Using symlinked version of '" ++ (packageId p) ++ "' from local cache."
  lsRepo = LS.addPackages LS.emptySet (allPackages repo) LS.FromRepository
  -- Find all packages that are in the global cache, but not in the repo
  newInGC = filter (\p -> not $ elemBy (packageIdEq p) (allPackages repo)) $ GC.allPackages gc
  lsGC = LS.addPackages lsRepo newInGC LS.FromGlobalCache
  filterGCLinked p = do
    points <- LocalCache.doesLinkPointToGlobalCache cfg gc dir (packageId p)
    return $ if points
      then Nothing
      else Just p

--- Resolves dependencies for a package.
resolveDependenciesForPackage :: Package -> Repository -> GC.GlobalCache 
                              -> IO (ErrorLogger ResolutionResult)
resolveDependenciesForPackage pkg repo gc = 
  resolveDependenciesFromLookupSet pkg lookupSet
 where
  lsRepo = LS.addPackages LS.emptySet (allPackages repo) LS.FromRepository
  -- Find all packages that are in the global cache, but not in the repo
  newInGC = filter inGCButNotInRepo $ GC.allPackages gc
  inGCButNotInRepo p = not $ elemBy (packageIdEq p) (allPackages repo)
  lookupSet = LS.addPackages lsRepo newInGC LS.FromGlobalCache

--- Acquires a package and its dependencies and installs them to the global
--- package cache.
acquireAndInstallPackageWithDependencies :: Config -> Repository 
                                         -> GC.GlobalCache -> Package 
                                         -> IO (ErrorLogger ())
acquireAndInstallPackageWithDependencies cfg repo gc pkg = 
  resolveDependenciesForPackage pkg repo gc |>=
  \result -> GC.installMissingDependencies cfg gc (resolvedPackages result) |>
  GC.acquireAndInstallPackage cfg pkg

elemBy :: (a -> Bool) -> [a] -> Bool
elemBy _ [] = False
elemBy f (x:xs) = if f x 
  then True
  else elemBy f xs

--- Links the dependencies of a package to its local cache and copies them to
--- its runtime cache.
copyDependencies :: Config -> GC.GlobalCache -> Package -> [Package] -> String 
                 -> IO (ErrorLogger ())
copyDependencies cfg gc pkg pkgs dir = 
  LocalCache.linkPackages cfg dir gc pkgs |>
  RuntimeCache.copyPackages cfg pkgs' dir >> succeedIO ()
 where 
  pkgs' = filter (/= pkg) pkgs

--- Upgrades all dependencies of a package copy.
upgradeAllPackages :: Config -> Repository -> GC.GlobalCache -> String 
                   -> IO (ErrorLogger ())
upgradeAllPackages cfg repo gc dir = loadPackageSpec dir |>=
  \pkgSpec -> LocalCache.clearCache dir >> succeedIO () |>
  installLocalDependencies cfg repo gc dir |>=
  \ (_,deps) -> copyDependencies cfg gc pkgSpec deps dir

--- Upgrades a single dependencies and its transitive dependencies.
upgradeSinglePackage :: Config -> Repository -> GC.GlobalCache -> String 
                     -> String -> IO (ErrorLogger ())
upgradeSinglePackage cfg repo gc dir pkgName = loadPackageSpec dir |>=
  \pkgSpec -> lookupSetForPackageCopy cfg pkgSpec repo gc dir |>=
  \originalLS -> let transitiveDeps = pkgName : allTransitiveDependencies originalLS pkgName in
  resolveDependenciesFromLookupSet pkgSpec (LS.setLocallyIgnored originalLS transitiveDeps) |>=
  \result -> GC.installMissingDependencies cfg gc (resolvedPackages result) |>
  log Info (showDependencies result) |>
  copyDependencies cfg gc pkgSpec (resolvedPackages result) dir

--- Installs the dependencies of a package.
installLocalDependencies :: Config -> Repository -> GC.GlobalCache -> String 
                         -> IO (ErrorLogger (Package,[Package]))
installLocalDependencies cfg repo gc dir =
  loadPackageSpec dir |>= \pkgSpec ->
  resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir |>= \result ->
  GC.installMissingDependencies cfg gc (resolvedPackages result) |>
  log Info (showDependencies result) |> 
  copyDependencies cfg gc pkgSpec (resolvedPackages result) dir |>
  succeedIO (pkgSpec, resolvedPackages result)

--- Links a directory into the local package cache. Used for cpm link.
linkToLocalCache :: String -> String -> IO (ErrorLogger ())
linkToLocalCache src pkgDir = do
  dirExists <- doesDirectoryExist src
  if dirExists
    then loadPackageSpec src |>=
      \pkgSpec -> LocalCache.createLink pkgDir src (packageId pkgSpec) True |> 
        succeedIO ()
    else log Critical ("Directory '" ++ src ++ "' does not exist.") |>
         succeedIO ()

--- Tries to find a package specification in the current directory or one of its
--- ancestors.
tryFindLocalPackageSpec :: String -> IO (ErrorLogger String)
tryFindLocalPackageSpec dir = do
  existsLocal <- doesFileExist $ dir </> "package.json"
  if existsLocal
    then succeedIO dir
    else log Debug ("No package.json in " ++ show dir ++ ", trying " ++
                    show (dir </> "..")) |> do
      parentExists <- doesDirectoryExist $ dir </> ".."
      if parentExists
        then tryFindLocalPackageSpec $ dir </> ".."
        else failIO "No package.json found"

--- Resolves the dependencies for a package copy and fills the package caches.
resolveAndCopyDependencies :: Config -> Repository -> GC.GlobalCache -> String 
                           -> IO (ErrorLogger [Package])
resolveAndCopyDependencies cfg repo gc dir =
  loadPackageSpec dir |>= resolveAndCopyDependenciesForPackage cfg repo gc dir

--- Resolves the dependencies for a package copy and fills the package caches.
resolveAndCopyDependenciesForPackage ::
     Config -> Repository -> GC.GlobalCache -> String -> Package
  -> IO (ErrorLogger [Package])
resolveAndCopyDependenciesForPackage cfg repo gc dir pkgSpec =
  resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir |>= \result -> 
    let deps = resolvedPackages result
        missingDeps = GC.missingPackages gc deps 
        failMsg = "Missing dependencies " 
                  ++ (intercalate "," $ map packageId missingDeps) 
                  ++ "\nUse `cpm install` to install missing dependencies."
    in if length missingDeps > 0
         then failIO failMsg
         else copyDependencies cfg gc pkgSpec deps dir |>= \_ ->
              succeedIO deps

--- Resolves the dependencies for a package copy.
resolveDependencies :: Config -> Repository -> GC.GlobalCache -> String 
                    -> IO (ErrorLogger ResolutionResult)
resolveDependencies cfg repo gc dir = loadPackageSpec dir |->
  log Info ("Read package spec from " ++ dir) |>=
  \pkgSpec -> resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir

--- Renders information on a package.
renderPackageInfo :: Bool -> Repository -> GC.GlobalCache -> Package -> String
renderPackageInfo allinfos _ gc pkg = pPrint doc
 where
  maxLen = 12
  doc = vcat $ [ heading, rule, installed, ver, auth, maintnr, synop
               , cats, deps, compilers, descr ] ++
               if allinfos
                 then [ srcdirs, expmods, cfgmod, execspec] ++ testsuites ++
                      [ src, licns, copyrt, homepg, reposy, bugrep]
                 else []

  pkgId = packageId pkg
  isInstalled = GC.isPackageInstalled gc pkg

  heading   = text pkgId 
  installed = if isInstalled then empty else red $ text "Not installed"
  rule      = text (take (length pkgId) $ repeat '-')
  ver       = fill maxLen (bold (text "Version")) <+>
              (text $ showVersion $ version pkg)
  auth      = fill maxLen (bold (text "Author")) <+> (text $ author pkg)
  synop     = fill maxLen (bold (text "Synopsis")) <+> (text $ synopsis pkg)
  deps      = (bold $ text "Dependencies") <$$>
              (vcat $ map (indent 4 . text . showDependency) $ dependencies pkg)

  maintnr = case maintainer pkg of
    Nothing -> empty
    Just  s -> fill maxLen (bold (text "Maintainer")) <+> text s

  cats =
    if null (category pkg)
      then empty
      else bold (text "Category") <+>
           indent 4 (fillSep (map text (category pkg)))

  execspec = case executableSpec pkg of
    Nothing -> empty
    Just  (PackageExecutable n m) ->
      bold (text "Executable") <$$>
      indent 4 (bold (text "Name         ") <+> text n) <$$>
      indent 4 (bold (text "Main module  ") <+> text m)

  testsuites = case testSuite pkg of
    Nothing -> []
    Just  tests ->
      map (\ (PackageTest dir mods opts) ->
             bold (text "Test suite") <$$>
             indent 4 (bold (text "Directory    ") <+> text dir) <$$>
             (if null opts
                then empty
                else indent 4 (bold (text "Check options") <+> text opts)) <$$>
             indent 4 (bold (text "Test modules ") <+>
             align (fillSep (map text mods))))
          tests

  descr  = showParaField description  "Description"
  licns  = showParaField license      "License"
  copyrt = showParaField copyright    "Copyright"
  homepg = showLineField homepage     "Homepage"
  reposy = showLineField repository   "Repository"
  bugrep = showLineField bugReports   "Bug reports"
  cfgmod = showLineField configModule "Config module"

  src = case source pkg of
    Nothing              -> empty
    Just  (Http s)       -> bold (text "Source") <$$> indent 4 (text s)
    Just  (Git s _)      -> bold (text "Source") <$$> indent 4 (text s)
    Just  (FileSource s) -> bold (text "Source") <$$> indent 4 (text s)

  srcdirs =
    if null (sourceDirs pkg)
      then empty
      else bold (text "Source directories") <$$>
           indent 4 (fillSep (map text (sourceDirs pkg)))

  expmods =
    if null (exportedModules pkg)
      then empty
      else bold (text "Exported modules") <$$>
           indent 4 (fillSep (map text (exportedModules pkg)))

  compilers =
    if null (compilerCompatibility pkg)
      then empty
      else bold (text "Compiler compatibility") <$$>
           (vcat $ map (indent 4 . text . showCompilerDependency)
                 $ compilerCompatibility pkg)

  showLineField fgetter fname = case fgetter pkg of
    Nothing -> empty
    Just  s -> bold (text fname) <$$> indent 4 (text s)

  showParaField fgetter fname = case fgetter pkg of
    Nothing -> empty
    Just  s -> bold (text fname) <$$>
               indent 4 (fillSep (map text (words s)))

