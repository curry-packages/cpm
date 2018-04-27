--------------------------------------------------------------------------------
--- This module contains some operations for processing packages,
--- like installing package sources, cleaning packages,
--- rendering package infos.
--------------------------------------------------------------------------------

module CPM.Package.Helpers
  ( installPackageSourceTo
  , renderPackageInfo
  , cleanPackage
  , getLocalPackageSpec, searchLocalPackageSpec
  ) where

import Directory
import Distribution ( addCurrySubdir )
import FilePath
import List         ( splitOn, nub )
import Pretty hiding ((</>))

import CPM.ErrorLogger
import CPM.FileUtil ( inDirectory, inTempDir, quote
                    , removeDirectoryComplete, tempDir, whenFileExists )
import CPM.Helpers  ( strip )
import CPM.Package

------------------------------------------------------------------------------
--- Installs the source of the package from the given source location
--- into the subdirectory `packageId pkg` of the given directory.
installPackageSourceTo :: Package -> PackageSource -> String
                         -> IO (ErrorLogger ())
--- 
--- @param pkg        - the package specification of the package
--- @param source     - the source of the package
--- @param installdir - the directory where the package subdirectory should be
---                     installed
installPackageSourceTo pkg (Git url rev) installdir = do
  let pkgDir = installdir </> pkgid
  c <- inDirectory installdir $ execQuietCmd cloneCommand
  if c == 0
    then case rev of
      Nothing           -> checkoutGitRef pkgDir "HEAD"
      Just (Tag tag)    -> checkoutGitRef pkgDir
                                          (replaceVersionInTag pkg tag)
      Just (Ref ref)    -> checkoutGitRef pkgDir ref
      Just VersionAsTag ->
        let tag = "v" ++ (showVersion $ version pkg) 
        in checkoutGitRef pkgDir tag |> 
           log Info ("Package '" ++ packageId pkg ++ "' installed")
    else removeDirectoryComplete pkgDir >>
         failIO ("Failed to clone repository from '" ++ url ++
                 "', return code " ++ show c)
 where
  pkgid  = packageId pkg
  cloneCommand q = unwords ["git clone", q, quote url, quote $ pkgid]

installPackageSourceTo pkg (FileSource zipfile) installdir =
  installPkgFromFile pkg zipfile (installdir </> packageId pkg) False

installPackageSourceTo pkg (Http url) installdir = do
  let pkgDir = installdir </> packageId pkg
      revurl  = reverse url
      pkgfile = if take 4 revurl == "piz."    then "package.zip" else
                if take 7 revurl == "zg.rat." then "package.tar.gz" else ""
  if null pkgfile
    then failIO $ "Illegal URL (only .zip or .tar.gz allowed):\n" ++ url
    else do
      tmpdir <- tempDir
      let tmppkgfile = tmpdir </> pkgfile
      c <- inTempDir $ showExecCmd $ "curl -s -o " ++ tmppkgfile ++
                                     " " ++ quote url
      if c == 0
        then installPkgFromFile pkg tmppkgfile pkgDir True
        else failIO $ "`curl` failed with exit status " ++ show c

--- Installs a package from a .zip or .tar.gz file into the specified
--- package directory. If the last argument is true, the file will be
--- deleted after unpacking.
installPkgFromFile :: Package -> String -> String -> Bool -> IO (ErrorLogger ())
installPkgFromFile pkg pkgfile pkgDir rmfile = do
  let iszip = take 4 (reverse pkgfile) == "piz."
  absfile <- getAbsolutePath pkgfile
  createDirectory pkgDir
  c <- if iszip
         then inTempDir $ showExecCmd $ "unzip -qq -d " ++ quote pkgDir ++
                                        " " ++ quote absfile
         else inDirectory pkgDir $ showExecCmd $
                "tar -xzf " ++ quote absfile
  when rmfile (showExecCmd ("rm -f " ++ absfile) >> done)
  if c == 0
    then log Info $ "Installed " ++ packageId pkg
    else do removeDirectoryComplete pkgDir
            failIO ("Failed to unzip package, return code " ++ show c)

--- Checks out a specific ref of a Git repository and deletes
--- the Git auxiliary files (i.e., `.git` and `.gitignore`).
--- 
--- @param dir - the directory containing the repo
--- @param ref - the ref to check out
checkoutGitRef :: String -> String -> IO (ErrorLogger ())
checkoutGitRef dir ref = do
  c <- inDirectory dir $ execQuietCmd (\q -> unwords ["git checkout", q, ref])
  if c == 0
    then removeGitFiles >> succeedIO ()
    else removeDirectoryComplete dir >>
         failIO ("Failed to check out " ++ ref ++ ", return code " ++ show c)
 where
   removeGitFiles = do
     removeDirectoryComplete (dir </> ".git")
     let gitignore = dir </> ".gitignore"
     whenFileExists gitignore (removeFile gitignore)

------------------------------------------------------------------------------
--- Cleans auxiliary files in the local package, i.e., the package
--- containing the current working directory.
cleanPackage :: LogLevel -> IO (ErrorLogger ())
cleanPackage ll =
  getLocalPackageSpec "." |>= \specDir ->
  loadPackageSpec specDir |>= \pkg ->
  let dotcpm   = specDir </> ".cpm"
      srcdirs  = map (specDir </>) (sourceDirsOf pkg)
      testdirs = map (specDir </>)
                     (maybe []
                            (map (\ (PackageTest m _ _ _) -> m))
                            (testSuite pkg))
      rmdirs   = nub (dotcpm : map addCurrySubdir (srcdirs ++ testdirs))
  in log ll ("Removing directories: " ++ unwords rmdirs) |>
     (showExecCmd (unwords $ ["rm", "-rf"] ++ rmdirs) >> succeedIO ())

------------------------------------------------------------------------------
--- Renders information about a package.
renderPackageInfo :: Bool -> Bool -> Maybe Bool -> Package -> String
renderPackageInfo allinfos plain mbinstalled pkg = pPrint doc
 where
  boldText s = (if plain then id else bold) $ text s
  maxLen = 12
  doc = vcat $ [ heading, rule
               , maybe empty instTxt mbinstalled
               , ver, auth, maintnr, synop
               , cats, deps, compilers, descr, execspec ] ++
               if allinfos
                 then [ srcdirs, expmods, cfgmod ] ++ testsuites ++
                      [ docuspec, src, licns, licfl, copyrt, homepg
                      , reposy, bugrep ]
                 else []

  pkgId = packageId pkg

  heading   = text pkgId 
  instTxt i = if i || plain then empty
                            else red $ text "Not installed"
  rule      = text (take (length pkgId) $ repeat '-')
  ver       = fill maxLen (boldText "Version") <+>
              (text $ showVersion $ version pkg)
  auth      = fill maxLen (boldText "Author") <+>
              indent 0 (fillSep (map (text . strip) (splitOn "," $ author pkg)))
  synop     = fill maxLen (boldText "Synopsis") <+>
              indent 0 (fillSep (map text (words (synopsis pkg))))
  deps      = boldText "Dependencies" <$$>
              (vcat $ map (indent 4 . text . showDependency) $ dependencies pkg)

  maintnr = case maintainer pkg of
    Nothing -> empty
    Just  s -> fill maxLen (boldText "Maintainer") <+>
               indent 0 (fillSep (map (text . strip) (splitOn "," s)))

  cats =
    if null (category pkg)
      then empty
      else fill maxLen (boldText "Category") <+>
           indent 0 (fillSep (map text (category pkg)))

  execspec = case executableSpec pkg of
    Nothing -> empty
    Just  (PackageExecutable n m eopts) ->
      if allinfos
        then boldText "Executable" <$$>
             indent 4 (boldText "Name         " <+> text n) <$$>
             indent 4 (boldText "Main module  " <+> text m) <$$>
             if null eopts
               then empty
               else indent 4 (boldText "Options      ") <+>
                    align (vsep (map (\ (c,o) -> text $ c ++ ": " ++ o) eopts))
        else fill maxLen (boldText "Executable") <+> text n

  testsuites = case testSuite pkg of
    Nothing -> []
    Just  tests ->
      map (\ (PackageTest dir mods opts script) ->
            let check = if null script then "Check" else "Test" in
            boldText "Test suite" <$$>
            indent 4 (boldText "Directory    " <+> text dir) <$$>
            (if null script
               then empty
               else indent 4 (boldText "Test script  " <+> text script)) <$$>
            (if null opts
               then empty
               else indent 4 (boldText (check++" options") <+>
                              text opts)) <$$>
            (if null mods
               then empty
               else indent 4 (boldText "Test modules " <+>
                    align (fillSep (map text mods)))))
          tests

  docuspec = case documentation pkg of
    Nothing -> empty
    Just  (PackageDocumentation docdir docmain doccmd) ->
      boldText "Documentation" <$$>
      indent 4 (boldText "Directory    " <+> text docdir) <$$>
      indent 4 (boldText "Main file    " <+> text docmain) <$$>
      if null doccmd
        then empty
        else indent 4 (boldText "Command      ") <+> text doccmd

  descr  = showParaField description  "Description"
  licns  = showLineField license      "License"
  licfl  = showLineField licenseFile  "License file"
  copyrt = showParaField copyright    "Copyright"
  homepg = showLineField homepage     "Homepage"
  reposy = showLineField repository   "Repository"
  bugrep = showLineField bugReports   "Bug reports"
  cfgmod = showLineField configModule "Config module"

  src = maybe empty
              (\_ -> boldText "Source" <$$>
                     indent 4 (text $ showPackageSource pkg))
              (source pkg)

  srcdirs =
    if null (sourceDirs pkg)
      then empty
      else boldText "Source directories" <$$>
           indent 4 (fillSep (map text (sourceDirs pkg)))

  expmods =
    if null (exportedModules pkg)
      then empty
      else boldText "Exported modules" <$$>
           indent 4 (fillSep (map text (exportedModules pkg)))

  compilers =
    if null (compilerCompatibility pkg)
      then empty
      else boldText "Compiler compatibility" <$$>
           (vcat $ map (indent 4 . text . showCompilerDependency)
                 $ compilerCompatibility pkg)

  showLineField fgetter fname = case fgetter pkg of
    Nothing -> empty
    Just  s -> boldText fname <$$> indent 4 (text s)

  showParaField fgetter fname = case fgetter pkg of
    Nothing -> empty
    Just  s -> boldText fname <$$>
               indent 4 (fillSep (map text (words s)))

------------------------------------------------------------------------------
--- Tries to find a package specification in the current directory or one of its
--- ancestors.
getLocalPackageSpec :: String -> IO (ErrorLogger String)
getLocalPackageSpec dir =
  searchLocalPackageSpec dir |>=
  maybe (failIO "No package.json found") succeedIO

--- Tries to find a package specification in the current directory or one of its
--- ancestors. Returns `Nothing` if there is not package specifiction.
searchLocalPackageSpec :: String -> IO (ErrorLogger (Maybe String))
searchLocalPackageSpec dir = do
  existsLocal <- doesFileExist $ dir </> "package.json"
  if existsLocal
    then succeedIO (Just dir)
    else log Debug ("No package.json in " ++ show dir ++ ", trying " ++
                    show (dir </> "..")) |> do
      parentExists <- doesDirectoryExist $ dir </> ".."
      if parentExists
        then searchLocalPackageSpec $ dir </> ".."
        else succeedIO Nothing

------------------------------------------------------------------------------
