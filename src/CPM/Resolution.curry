--------------------------------------------------------------------------------
--- This module contains the dependency resolution algorithm.
--------------------------------------------------------------------------------

module CPM.Resolution 
  ( ResolutionResult
  , showResult
  , resolutionSuccess
  , resolvedPackages
  , showDependencies
  , showConflict
  , allTransitiveDependencies
  , transitiveDependencies
  , resolve
  , resolveDependenciesFromLookupSet
  , isCompatibleToCompiler
  , isDisjunctionCompatible
  ) where

import Either
import List
import Sort
import Maybe
import Test.Prop
import Text.Pretty

import CPM.Config      ( Config, defaultConfig, compilerVersion
                       , compilerBaseVersion )
import CPM.ErrorLogger
import CPM.LookupSet
import CPM.Package

--- Resolves the dependencies of a package using packages from a lookup set, 
--- inside an error logger.
resolveDependenciesFromLookupSet :: Config -> Package -> LookupSet
                                 -> IO (ErrorLogger ResolutionResult)
resolveDependenciesFromLookupSet cfg pkg lookupSet = 
  let result = resolve cfg pkg lookupSet in if resolutionSuccess result
    then succeedIO result
    else failIO $ showResult result

--- Resolves the dependencies of a package using packages from a lookup set.
--- The base package of the current compiler is removed from the result set.
resolve :: Config -> Package -> LookupSet -> ResolutionResult
resolve cfg pkg ls = case resolvedPkgs of
  Just pkgs -> ResolutionSuccess pkg (deleteBase pkgs)
  Nothing   -> ResolutionFailure labeledTree
 where
  labeledTree = labelConflicts cfg $ candidateTree pkg ls
  noConflicts = prune ((/= Nothing) . clConflict) labeledTree
  resolvedPkgs = maybeHead . map stPackages . filter stComplete . leaves
                           . mapTree clState $ noConflicts
  deleteBase   = filter (\p -> name p /= "base" ||
                           showVersion (version p) /= compilerBaseVersion cfg)

--- Gives a list of all activated packages for a successful resolution.
resolvedPackages :: ResolutionResult -> [Package]
resolvedPackages (ResolutionSuccess pkg deps) = delete pkg deps
resolvedPackages (ResolutionFailure _) = error "resolvedPackages called on failure"

--- Tries to get a list of activated packages for a resolution. Returns Nothing
--- if the resolution was not successful.
maybeResolvedPackages :: ResolutionResult -> Maybe [Package]
maybeResolvedPackages (ResolutionSuccess _ deps) = Just deps
maybeResolvedPackages (ResolutionFailure _) = Nothing

--- Was a resolution successful?
resolutionSuccess :: ResolutionResult -> Bool
resolutionSuccess (ResolutionSuccess _ _) = True
resolutionSuccess (ResolutionFailure _) = False

--- Renders a dependency tree from a successful resolution.
showDependencies :: ResolutionResult -> String
showDependencies (ResolutionSuccess pkg deps) =
  showTree . mapTree (text . packageId) $ dependencyTree deps pkg
showDependencies (ResolutionFailure _) = "Resolution failed."

--- Renders a conflict resolution into a textual representation.
showConflict :: ResolutionResult -> String
showConflict (ResolutionSuccess _ _) = "Resolution succeeded."
showConflict (ResolutionFailure t) = case findRelevantConflict t of
  Just c  -> showConflictState c
  Nothing -> case missingPackages $ clState $ findDeepestNode t of
    []    -> "Conflict resolution failed for an unknown reason... Hint:(\n" ++
             "Please clean your package ('cypm clean') and/or\n" ++
             "your package index ('cypm update') and try again..."
    (d@(Dependency p _):_) ->
      "There seems to be no version of package " ++ p ++
      " that can satisfy the constraint " ++ showDependency d

showConflictTree :: ResolutionResult -> String
showConflictTree (ResolutionSuccess _ _) = "Resolution succeeded."
showConflictTree (ResolutionFailure t) =
  showTree $ mapTree labeler $ cutBelowConflict t
 where
  pkgId = text . packageId . actPackage
  actChain a@(InitialA _) = pkgId a
  actChain a@(ChildA _ _ p) = pkgId a <+> text "->" <+> actChain p
  labeler ((a, _), Nothing) = pkgId a
  labeler ((a, _), Just (CompilerConflict _)) = red $ text "C" <+> actChain a
  labeler ((a, _), Just (PrimaryConflict _)) = red $ text "P" <+> actChain a
  labeler ((a, _), Just (SecondaryConflict a' a'')) = 
    red $ text "S" <+> actChain a <+> parens (pkgId a') <+> parens (pkgId a'')
  cutBelowConflict (Node (a, Nothing) cs) = Node (a, Nothing) $ map cutBelowConflict cs
  cutBelowConflict (Node (a, Just  c)  _) = Node (a, Just  c) []

showCandidateTree :: Tree State -> String
showCandidateTree = showTree . mapTree (text . packageId . actPackage . stActivation)

showLabelTree :: Config -> Tree State -> String
showLabelTree cfg =
  showTree . mapTree labeler . cutBelowConflict . labelConflicts cfg
 where
  pkgId = text . packageId . actPackage . stActivation
  actId = text . packageId . actPackage
  labeler (s, Nothing) = pkgId s
  labeler (s, Just (CompilerConflict _)) = red $ text "C" <+> pkgId s
  labeler (s, Just (PrimaryConflict _)) = red $ text "P" <+> pkgId s
  labeler (s, Just (SecondaryConflict a1 a2)) = red $ text "S" <+> pkgId s <+> actId a1 <+> actId a2
  cutBelowConflict (Node (a, Nothing) cs) = Node (a, Nothing) $ map cutBelowConflict cs
  cutBelowConflict (Node (a, Just  c)  _) = Node (a, Just  c) []

resultConflict :: ResolutionResult -> Maybe Conflict
resultConflict (ResolutionSuccess _ _) = Nothing
resultConflict (ResolutionFailure t) = case findRelevantConflict t of
  Nothing -> Nothing
  Just cs -> clConflict cs

--- Renders a resolution result into a textual representation for the user. In
--- case of success, the dependency tree is shown. In case of failure, 
--- information on the cause of the conflict is shown.
showResult :: ResolutionResult -> String
showResult r@(ResolutionSuccess _ _) = showDependencies r
showResult r@(ResolutionFailure _)   = showConflict r

--- Result of a resolution run. In case of success, it contains the original
--- package as well as a list of resolved packages. If the resolution failed, it
--- contains the conflict tree.
data ResolutionResult = ResolutionSuccess Package [Package] 
                      | ResolutionFailure (Tree ConflictState)
 deriving (Eq,Show)

--- Represents an activation of a package in the candidate tree. Activations 
--- form a chain up to the initial activation, i.e. the initial package that 
--- resolution was started on. Each activation's parent is the activation of the
--- package that led to the current activation, i.e. the package whose 
--- dependency led to the current package version being chosen.
data Activation = InitialA Package
                | ChildA Package Dependency Activation
 deriving (Eq,Show)

--- Each tree node is labeled with the current activation and all former 
--- activations.
type State = (Activation, [Activation])

--- A conflict occurs when one of the active packages in a state clashes with
--- one of dependencies of all of the state's active packages. If the clash 
--- occurs between a package A and a dependency of a package B and B is also the
--- package that activated A, i.e. the parent of its activation, we call the 
--- conflict a 'same package' conflict. A 'real' conflict is a one where package
--- A was activated by some earlier package. When the compiler compatibility 
--- constraints of the package activated in the current state are not met, then
--- we use the compiler conflict.
data Conflict = SecondaryConflict Activation Activation
              | PrimaryConflict Activation
              | CompilerConflict Activation
 deriving (Eq,Show)

--- A state and a potential conflict.
type ConflictState = (State, Maybe Conflict)

--- Gets the package that was activated in a state.
stPackage :: State -> Package
stPackage (a, _) = actPackage a

--- Gets all active packages in a state.
stPackages :: State -> [Package]
stPackages (_, as) = map actPackage as 

--- Gets the state's current activation
stActivation :: State -> Activation
stActivation = fst

--- Gets all activations leading up to the state, including the current 
--- activation.
stActivations :: State -> [Activation]
stActivations = snd

--- Gets a list of all dependencies of all active packages in a state, alongside
--- the activations that activated the respective packages.
stDependencies :: State -> [(Activation, Dependency)]
stDependencies = concatMap zippedDeps . stActivations
 where
  zippedDeps a = zip (repeat a) $ dependencies $ actPackage a

--- Gets a list of all dependencies of all active packages in a state.
stAllDependencies :: State -> [Dependency]
stAllDependencies = concatMap dependencies . stPackages

--- Gets an activation's package.
actPackage :: Activation -> Package
actPackage (InitialA p) = p
actPackage (ChildA p _ _) = p

actDependency :: Activation -> Dependency
actDependency (InitialA _) = error "Called on initialA"
actDependency (ChildA _ d _) = d

actParent :: Activation -> Activation
actParent a@(InitialA _) = a
actParent (ChildA _ _ p) = p

--- Gets a potential conflict from a conflict state.
clConflict :: ConflictState -> Maybe Conflict
clConflict = snd

--- Gets the original state from a conflict state.
clState :: ConflictState -> State
clState = fst

--- A tree with a label and child nodes.
data Tree a = Node a [Tree a]
 deriving (Eq,Show)

--- Recursively applies a function to each node in a tree.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node a cs) = Node (f a) $ map (mapTree f) cs

--- A node's label.
label :: Tree a -> a
label (Node a _) = a

leaves :: Tree a -> [a]
leaves (Node a []) = [a]
leaves (Node _ cs@(_:_)) = concatMap leaves cs

--- Folds a tree to a value.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a cs) = f a (map (foldTree f) cs)

--- Filters a tree using a predicate.
filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree p = foldTree f
 where f a cs = Node a (filter (p . label) cs)

--- Removes all nodes from a tree that match the predicate.
prune :: (a -> Bool) -> Tree a -> Tree a
prune p = filterTree (not . p)

--- Shows a textual representation of a tree.
showTree :: Tree Doc -> String
showTree t = "Package dependencies:\n" ++ pPrint (ppTree t)

--- Pretty prints a tree of Docs into a single Doc.
ppTree :: Tree Doc -> Doc
ppTree (Node l cs) = l <$$> vcat children
 where
  children = map (\t -> indent 2 $ text "|-" <+> ppTree t) cs

--- Extends a tree by appending a node to the first leaf in order, i.e. the 
--- leftmost leaf.
extendTree :: Tree a -> Tree a -> Tree a
extendTree (Node a []) n = Node a [n]
extendTree (Node a (c:cs)) n = Node a $ (extendTree c n):cs

--- Converts a tree of strings into the Graphviz dot format.
dotifyTree :: Tree String -> String
dotifyTree t = "digraph tree {\n" ++ full ++ "\n}"
 where
  (_, _, full) = dotify' (0, [], "") t
  dotify' (n, acc, s) (Node l cs) = 
    let
      (n', children, str) = foldl (dotify') (n + 1, [], "") cs
    in
      (n', n:acc, s ++ intercalate "\n" ([node n l] ++ map (edge n) children) ++ str)
  node n l = "n" ++ (show n) ++ " [label=\"" ++ l ++ "\"];\n"
  edge a b = "n" ++ (show a) ++ " -> " ++ "n" ++ (show b) ++ ";\n"

--- Builds a tree of candidate states from a package and a lookup set. This is
--- the tree that is searched for complete states or conflicts.
candidateTree :: Package -> LookupSet -> Tree State
candidateTree pkg ls = let s = InitialA pkg in
  Node (s, [s]) $ tree' [s] (zip (repeat s) (dependencies pkg))
 where
  tree' acts ((act, d@(Dependency p _)):ds) = 
    if p `elem` (map (name . actPackage) acts)
      then tree' acts ds 
      else map (nodesForDep act d ds acts) $ findAllVersions ls p True
  tree' _ [] = []

  nodesForDep act d ds acts p' = 
    let 
      act' = ChildA p' d act 
      acts' = act':acts
      nextDeps = zip (repeat act') (dependencies p') ++ ds
    in Node (act', acts') $ tree' acts' nextDeps

--- Calculates the first conflict for each node in the tree and annotates the 
--- nodes with these conflicts.
labelConflicts :: Config -> Tree State -> Tree ConflictState
labelConflicts cfg = mapTree f
 where
  f s = (s, firstConflict cfg s (reverse $ stDependencies s))

--- Checks whether a state is complete, i.e. whether all packages mentioned in
--- all dependencies of all active packages are present in the list of active
--- packages. Note that stComplete does not check whether a dependency is 
--- actually met by a package, only whether the package is present. stComplete
--- is meant to be called on a state that has already been checked for 
--- conflicts.
stComplete :: State -> Bool
stComplete s = missingPackages s == []

stCompleteness :: State -> Int
stCompleteness s = length $ missingPackages s

--- Finds all dependencies in a state which is unmet because its dependency is
--- missing altogether, i.e. no version of the package is activated.
missingPackages :: State -> [Dependency]
missingPackages s = missing' (stPackages s) (stAllDependencies s)
 where
  missing' pkgs ds = filter (noPackage pkgs) ds
  noPackage pkgs (Dependency p _) = find ((== p) . name) pkgs == Nothing

--- Calculates the first conflict in a state.
firstConflict :: Config -> State -> [(Activation, Dependency)] -> Maybe Conflict
firstConflict _ _ [] = Nothing
firstConflict cfg s@(act, acts) ((depAct, Dependency p disj):ds) =
  if not $ isCompatibleToCompiler cfg (actPackage act)
    then Just $ CompilerConflict act
    else case findPkg of
      Nothing -> firstConflict cfg s ds
      Just  a -> if isDisjunctionCompatible (version $ actPackage a) disj
        then firstConflict cfg s ds
        else if actParent a == depAct
          then Just $ PrimaryConflict a
          else Just $ SecondaryConflict a depAct
 where
  findPkg = find ((== p) . name . actPackage) acts

--- Finds the deepest right-most node in a tree.
findDeepestNode :: Tree a -> a
findDeepestNode = snd . maxNode . leaves . depthTree
 where
  maxNode ls = foldl maxN (head ls) ls
  maxN (na, a) (nb, b) = if nb >= na 
    then (nb, b)
    else (na, a)
  depthTree = relabel 0
  relabel n (Node a cs) = Node (n, a) (map (relabel (n + 1)) cs)

findRelevantConflict :: Tree ConflictState -> Maybe ConflictState
findRelevantConflict = maybeMostRelevant . map mostRelevant . map snd . minGroups . filter ((/= []) . snd) . findGroups . cutBelowConflict . relabel 
 where
  maybeMostRelevant [] = Nothing
  maybeMostRelevant cs@(_:_) = Just $ mostRelevant cs
  mostRelevant cs = case find (isSecondary . fromJust . clConflict) cs of
    Just s -> s
    Nothing -> case find (isCompiler . fromJust . clConflict) cs of
      Just c -> c
      Nothing -> head cs
  minGroups gs = let minG = foldl (\m g -> min m (fst g)) 99999 gs in
    filter ((== minG) . fst) gs
  isSecondary (SecondaryConflict _ _) = True
  isSecondary (PrimaryConflict _) = False
  isSecondary (CompilerConflict _) = False
  isCompiler (SecondaryConflict _ _) = False
  isCompiler (PrimaryConflict _) = False
  isCompiler (CompilerConflict _) = True
  findGroups (Node (d, (_, Nothing)) []) = [(d, [])]
  findGroups (Node (d, (_, Nothing)) cs@(_:_)) = if containsOnlyConflicts cs
    then [(d, map (snd . label) cs)]
    else concatMap findGroups cs
  findGroups (Node (d, (_, Just  _)) _) = [(d, [])]
  containsOnlyConflicts = all (isJust . clConflict .  snd) . map label 
  cutBelowConflict (Node (d, (a, Nothing)) cs) = Node (d, (a, Nothing)) $ map cutBelowConflict cs
  cutBelowConflict (Node (d, (a, Just  c))  _) = Node (d, (a, Just  c)) []
  relabel = mapTree (\a -> (stCompleteness $ clState a, a))

--- Renders the information from a real conflict into a textual representation 
--- for the user.
---
--- @param originalAct - the original activation of the package
--- @param confDep - the dependency conflicting the original activation
--- @param confAct - the activation of the conflict dependency
showRealConflictInfo :: Activation -> Activation -> String
showRealConflictInfo originalAct confAct = 
  let
    mkLabel pkg dep = (text $ name pkg) <+> (parens $ text $ showDependency dep)
    triedPkg = actPackage originalAct
    actLabeler (InitialA p) = text $ name p
    actLabeler (ChildA p dep _) = mkLabel p dep
    originalTree = mapTree actLabeler $ activationTree originalAct
    confTree = mapTree actLabeler $ activationTree confAct
    confDepLabel = mkLabel triedPkg (findDependencyOn triedPkg confAct)
    confTree' = extendTree confTree (Node confDepLabel [])
    findDependencyOn pkg act = case find ((== name pkg) . depPkg) $ dependencies $ actPackage act of
      Just a -> a
      Nothing -> error "Hey!"
    depPkg (Dependency p _) = p
  in
    pPrint $ (text $ "There was a conflict for package " ++ name triedPkg) 
      <$$> ppTree originalTree <$$> ppTree confTree'

showSamePackageConflictInfo :: Activation -> String
showSamePackageConflictInfo act =
  let
    triedPkg = actPackage act
  in
    "There seems to be no version of package " ++ name triedPkg ++ " that can satisfy the constraint " ++ showDependency (actDependency act) 

showCompilerConflictInfo :: Activation -> String
showCompilerConflictInfo act =
  "The package " ++ (packageId $ actPackage act) ++ ", dependency constraint " ++ showDependency (actDependency act) ++ ", is not compatible to the current compiler. It was activated because:\n" ++ showTree actTree
 where
  mkLabel pkg dep = (text $ name pkg) <+> (parens $ text $ showDependency dep)
  actLabeler (InitialA p) = text $ name p
  actLabeler (ChildA p dep _) = mkLabel p dep
  actTree = mapTree actLabeler $ activationTree act

--- Renders a conflict state into a textual representation for the user.
showConflictState :: ConflictState -> String
showConflictState ((InitialA _, _), Nothing) = "No Conflict!"
showConflictState ((InitialA pkg, _), Just _) = "Initial Conflict! " ++ packageId pkg
showConflictState ((ChildA _ _ _, _), Nothing) = "No Conflict!"
showConflictState ((ChildA _ _ _, _), Just (PrimaryConflict originalAct)) =
  showSamePackageConflictInfo originalAct 
showConflictState ((ChildA _ _ _, _), Just (SecondaryConflict originalAct confAct)) =
  showRealConflictInfo originalAct confAct
showConflictState ((ChildA _ _ _, _), Just (CompilerConflict act)) =
  showCompilerConflictInfo act

--- Turns an activation into a tree, with the initial activation as its root.
--- Note that this tree will be a singly linked list, i.e. each node will have
--- at most one child.
activationTree :: Activation -> Tree Activation
activationTree = head . foldl (\acc f -> f acc) [] . actTree'
 where
  actTree' x@(InitialA _) = [\cs -> [Node x cs]]
  actTree' x@(ChildA _ _ parent) = (\cs -> [Node x cs]):(actTree' parent)

--- Turns a list of activated packages and an original package into a dependency
--- tree.
dependencyTree :: [Package] -> Package -> Tree Package
dependencyTree chosen pkg = Node pkg $ map (dependencyTree chosen) childPkgs
 where
  justs = map fromJust . filter (/= Nothing)
  childPkgs = justs $ map findPkg (dependencies pkg)
  findPkg (Dependency p _) = find ((== p) . name) chosen

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

packageSource :: Package -> LookupSet -> Maybe LookupSource
packageSource p ls = lookupSource ls p

allTransitiveDependencies' :: [String] -> LookupSet -> String -> [String]
allTransitiveDependencies' seen ls pkg = nub $ allDeps
 where
  allVersions = findAllVersions ls pkg True
  allDeps = foldl (\s d ->  transitiveDependencies' s ls d) seen allVersions

allTransitiveDependencies :: LookupSet -> String -> [String]
allTransitiveDependencies = allTransitiveDependencies' []

transitiveDependencies' :: [String] -> LookupSet -> Package -> [String]
transitiveDependencies' seen ls pkg = foldl (\s d -> if d `elem` s then s else (nub (s ++ allTransitiveDependencies' (d:s) ls d))) seen deps
 where
  deps = map dependencyName $ dependencies pkg 
  dependencyName (Dependency n _) = n

transitiveDependencies :: LookupSet -> Package -> [String]
transitiveDependencies = transitiveDependencies' []

test_transitiveDependencies_simpleCase :: Prop
test_transitiveDependencies_simpleCase = transitiveDependencies db pkg -=- ["B", "C"]
  where pkg = cPackage "A" (0, 0, 1, Nothing) [cDep "B" ">= 1.0.0", cDep "C" "= 1.2.0"]
        b = cPackage "B" (1, 0, 9, Nothing) []
        c = cPackage "C" (1, 2, 0, Nothing) []
        db = cDB [b, c]

test_transitiveDependencies_loop :: Prop
test_transitiveDependencies_loop = transitiveDependencies db pkg -=- ["B", "C"]
  where pkg = cPackage "A" (0, 0, 1, Nothing) [cDep "B" ">= 1.0.0", cDep "C" "= 1.2.0"]
        b = cPackage "B" (1, 0, 0, Nothing) [cDep "C" "= 1.2.0"]
        c = cPackage "C" (1, 2, 0, Nothing) [cDep "B" ">= 1.0.0"]
        db = cDB [b, c]

test_transitiveDependencies_multipleVersions :: Prop
test_transitiveDependencies_multipleVersions = transitiveDependencies db pkg -=- ["B", "D", "C"]
  where pkg = cPackage "A" (0, 0, 1, Nothing) [cDep "B" ">= 1.0.0"]
        b100 = cPackage "B" (1, 0, 0, Nothing) [cDep "C" "= 1.0.0"]
        b110 = cPackage "B" (1, 1, 0, Nothing) [cDep "D" "= 1.0.0"]
        c = cPackage "C" (1, 0, 0, Nothing) []
        d = cPackage "D" (1, 0, 0, Nothing) []
        db = cDB [b100, b110, c, d]

isCompatibleToCompiler :: Config -> Package -> Bool
isCompatibleToCompiler cfg p = case compats of
  []    -> True -- No constraints => compiler is compatible
  (_:_) -> case constraintForCompiler of
    Nothing -> False -- No constraints for current compiler
                     -- => compiler is incompatible
    Just (CompilerCompatibility _ c) ->
               isDisjunctionCompatible (maj, min, revi, Nothing) c
 where
  (name, maj, min, revi) = compilerVersion cfg
  compats = compilerCompatibility p
  constraintForCompiler = find (\(CompilerCompatibility c _) -> c == name)
                               compats

isDisjunctionCompatible :: Version -> Disjunction -> Bool
isDisjunctionCompatible ver cs = any id (map (all id) rs)
 where
  rs = map (map isCompatible) cs
  preReleaseCompatible (_, _, _, p1) (_, _, _, p2) 
    = (isJust p1 && isJust p2) || (isNothing p1 && isNothing p2)
  isCompatible (VExact v) = v == ver
  isCompatible (VLt v) = ver `vlt` v && preReleaseCompatible ver v
  isCompatible (VLte v) = ver `vlte` v && preReleaseCompatible ver v
  isCompatible (VGt v) = ver `vgt` v && preReleaseCompatible ver v
  isCompatible (VGte v) = ver `vgte` v && preReleaseCompatible ver v
  isCompatible (VCompatible v) = ver `vgte` v && ver `vlt` (nextMinor v) &&
    preReleaseCompatible ver v
  nextMinor (maj, min, _, _) = (maj, min + 1, 0, Nothing)

test_onlyConjunctionCompatible :: Prop
test_onlyConjunctionCompatible = isDisjunctionCompatible ver dis -=- True
  where dis = cDisj "= 1.0.0"
        ver = (1, 0, 0, Nothing)

test_allConjunctionsCompatible :: Prop
test_allConjunctionsCompatible = isDisjunctionCompatible ver dis -=- True
  where dis = cDisj ">= 1.0.0 || = 1.2.0"
        ver = (1, 2, 0, Nothing)

test_oneConjunctionCompatible :: Prop
test_oneConjunctionCompatible = isDisjunctionCompatible ver dis -=- True
  where ver = (1, 0, 0, Nothing)
        dis = cDisj "> 2.0.0 || = 1.0.0"

test_conjunctionWithMultipleParts :: Prop
test_conjunctionWithMultipleParts = isDisjunctionCompatible ver dis -=- True
  where ver = (1, 0, 0, Nothing)
        dis = cDisj ">= 1.0.0, < 2.0.0"

test_reportsSimpleFailure :: Prop
test_reportsSimpleFailure = isDisjunctionCompatible ver dis -=- False
  where ver = (1, 0, 0, Nothing)
        dis = cDisj "> 1.0.0" 
  
test_reportsAllConjunctionsAsFailure :: Prop
test_reportsAllConjunctionsAsFailure = isDisjunctionCompatible ver dis -=- False
  where ver = (1, 0, 0, Nothing)
        dis = cDisj "< 1.0.0 || > 1.0.0"

test_reportsRelevantPartOfConjunction :: Prop
test_reportsRelevantPartOfConjunction = isDisjunctionCompatible ver dis -=- False
  where ver = (1, 0, 0, Nothing)
        dis = cDisj "< 1.0.0, > 0.5.0"
  
test_semverCompatible :: Prop
test_semverCompatible = isDisjunctionCompatible ver dis -=- True
  where ver = (0, 5, 9, Nothing)
        dis = cDisj "~> 0.5.0"

test_semverIncompatible :: Prop
test_semverIncompatible = isDisjunctionCompatible ver dis -=- False
  where ver = (0, 7, 1, Nothing)
        dis = cDisj "~> 0.6.0"

test_semverMinimum :: Prop
test_semverMinimum = isDisjunctionCompatible ver dis -=- False
  where ver = (0, 7, 0, Nothing)
        dis = cDisj "~> 0.7.2"
  
test_resolvesSimpleDependency :: Prop
test_resolvesSimpleDependency =
  maybeResolvedPackages (resolve defaultConfig pkg db) -=- Just [json100, pkg]
  where pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "=1.0.0"]
        json100 = cPackage "json" (1, 0, 0, Nothing) []
        json101 = cPackage "json" (1, 0, 1, Nothing) []
        db  = cDB [json100, json101]

test_reportsUnknownPackage :: Prop
test_reportsUnknownPackage = showResult result -=- "There seems to be no version of package json that can satisfy the constraint json = 1.0.0"
  where result = resolve defaultConfig pkg db
        pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "= 1.0.0"]
        db = cDB [pkg]

test_reportsMissingPackageVersion :: Prop
test_reportsMissingPackageVersion = showResult result -=- "There seems to be no version of package json that can satisfy the constraint json = 1.2.0"
  where result = resolve defaultConfig pkg db
        pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "=1.2.0"]
        json = cPackage "json" (1, 0, 0, Nothing) []
        db  = cDB [json]

test_reportsSecondaryConflict :: Prop
test_reportsSecondaryConflict = showResult result -=- expectedMessage
 where result = resolve defaultConfig pkg db
       pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "= 1.0.0", cDep "b" ">= 0.0.1"]
       b = cPackage "b" (0, 0, 2, Nothing) [cDep "json" "~> 1.0.4"]
       json100 = cPackage "json" (1, 0, 0, Nothing) []
       json105 = cPackage "json" (1, 0, 5, Nothing) []
       db = cDB [pkg, b, json100, json105]
       expectedMessage = "There was a conflict for package json\n"
        ++ "sample\n"
        ++ "  |- json (json = 1.0.0)\n"
        ++ "sample\n"
        ++ "  |- b (b >= 0.0.1)\n"
        ++ "    |- json (json ~> 1.0.4)"

test_reportsSecondaryConflictInsteadOfPrimary :: Prop
test_reportsSecondaryConflictInsteadOfPrimary = showResult result -=- expectedMessage
 where result = resolve defaultConfig pkg db
       pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "= 1.0.0", cDep "b" ">= 0.0.5"]
       b001 = cPackage "b" (0, 0, 1, Nothing) []
       b002 = cPackage "b" (0, 0, 2, Nothing) []
       b003 = cPackage "b" (0, 0, 3, Nothing) []
       b006 = cPackage "b" (0, 0, 6, Nothing) [cDep "json" "~> 1.0.4"]
       json100 = cPackage "json" (1, 0, 0, Nothing) []
       json105 = cPackage "json" (1, 0, 5, Nothing) []
       db = cDB [pkg, b001, b002, b003, b006, json100, json105]
       expectedMessage = "There was a conflict for package json\n"
        ++ "sample\n"
        ++ "  |- json (json = 1.0.0)\n"
        ++ "sample\n"
        ++ "  |- b (b >= 0.0.5)\n"
        ++ "    |- json (json ~> 1.0.4)"

test_detectsSecondaryOnFirstActivation :: Prop
test_detectsSecondaryOnFirstActivation = showResult result -=- expectedMessage
 where result = resolve defaultConfig pkg db
       pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "a" "= 0.0.1", cDep "b" "> 0.0.1"]
       a001 = cPackage "a" (0, 0, 1, Nothing) [cDep "b" "= 0.0.1"]
       b001 = cPackage "b" (0, 0, 1, Nothing) []
       b002 = cPackage "b" (0, 0, 2, Nothing) []
       db = cDB [pkg, a001, b001, b002]
       expectedMessage = "There was a conflict for package b\n"
        ++ "sample\n"
        ++ "  |- a (a = 0.0.1)\n"
        ++ "    |- b (b = 0.0.1)\n"
        ++ "sample\n"
        ++ "  |- b (b > 0.0.1)"

test_makesDecisionBetweenAlternatives :: Prop
test_makesDecisionBetweenAlternatives =
  maybeResolvedPackages (resolve defaultConfig pkg db) -=- Just [json150, pkg]
  where pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "> 1.0.0, < 2.0.0 || >= 4.0.0"]
        json150 = cPackage "json" (1, 5, 0, Nothing) []
        json320 = cPackage "json" (3, 2, 0, Nothing) []
        db = cDB [json150, json320]

test_alwaysChoosesNewestAlternative :: Prop
test_alwaysChoosesNewestAlternative =
  maybeResolvedPackages (resolve defaultConfig pkg db) -=- Just [json420, pkg]
  where pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "> 1.0.0, < 2.0.0 || >= 4.0.0"]
        json150 = cPackage "json" (1, 5, 0, Nothing) []
        json420 = cPackage "json" (4, 2, 0, Nothing) []
        db = cDB [json150, json420]

test_doesNotChoosePrereleaseByDefault :: Prop
test_doesNotChoosePrereleaseByDefault =
  maybeResolvedPackages (resolve defaultConfig pkg db) -=- Just [b109, pkg]
  where pkg = cPackage "A" (0, 0, 1, Nothing) [cDep "B" ">= 1.0.0"]
        b109 = cPackage "B" (1, 0, 9, Nothing) []
        b110b1 = cPackage "B" (1, 1, 0, Just "b1") []
        db = cDB [b109, b110b1]

test_upgradesPackageToPrereleaseWhenNeccesary :: Prop
test_upgradesPackageToPrereleaseWhenNeccesary =
  maybeResolvedPackages (resolve defaultConfig pkg db) -=- Just [b110b1, c, pkg]
  where pkg = cPackage "A" (0, 0, 1, Nothing) [cDep "C" "= 1.2.0"]
        b109 = cPackage "B" (1, 0, 9, Nothing) []
        b110b1 = cPackage "B" (1, 1, 0, Just "b1") []
        c = cPackage "C" (1, 2, 0, Nothing) [cDep "B" ">= 1.1.0-b1"]
        db = cDB [b109, b110b1, c]

test_prefersLocalPackageCacheEvenIfOlder :: Prop
test_prefersLocalPackageCacheEvenIfOlder =
  maybeResolvedPackages (resolve defaultConfig pkg db) -=- Just [b101, pkg]
  where pkg = cPackage "A" (0, 0, 1, Nothing) [cDep "B" ">= 1.0.0"]
        b101 = cPackage "B" (1, 0, 1, Nothing) []
        b105 = cPackage "B" (1, 0, 5, Nothing) []
        db = addPackage (addPackage emptySet b101 FromLocalCache) b105 FromRepository

test_reportsCompilerIncompatibility :: Prop
test_reportsCompilerIncompatibility = showResult result -=- "The package json-1.0.0, dependency constraint json = 1.0.0, is not compatible to the current compiler. It was activated because:\nPackage dependencies:\nsample\n  |- json (json = 1.0.0)"
  where result = resolve defaultConfig pkg db
        pkg = cPackage "sample" (0, 0, 1, Nothing) [cDep "json" "= 1.0.0"]
        json = cPackageCC "json" (1, 0, 0, Nothing) [cCC "nocompiler" "= 1.0.0"]
        db = cDB [json]

cPackage :: String -> Version -> [Dependency] -> Package
cPackage p v ds = emptyPackage {
    name = p
  , version = v
  , author = "author"
  , synopsis = "JSON library for Curry"
  , dependencies = ds
  , maintainer = Nothing
  , description = Nothing
  , license = Nothing
  , licenseFile = Nothing
  , copyright = Nothing
  , homepage = Nothing
  , bugReports = Nothing
  , repository = Nothing
  , compilerCompatibility = []
  , source = Nothing
  , exportedModules = []
  }

cPackageCC :: String -> Version -> [CompilerCompatibility] -> Package
cPackageCC p v cs = emptyPackage {
    name = p
  , version = v
  , author = "author"
  , synopsis = "JSON library for Curry"
  , dependencies = []
  , maintainer = Nothing
  , description = Nothing
  , license = Nothing
  , licenseFile = Nothing
  , copyright = Nothing
  , homepage = Nothing
  , bugReports = Nothing
  , repository = Nothing
  , compilerCompatibility = cs
  , source = Nothing
  , exportedModules = []
  }

cDisj :: String -> Disjunction
cDisj = fromJust . readVersionConstraints

cDep :: String -> String -> Dependency
cDep p c = Dependency p (fromJust $ readVersionConstraints c)

cCC :: String -> String -> CompilerCompatibility
cCC p c = CompilerCompatibility p (fromJust $ readVersionConstraints c)

cDB :: [Package] -> LookupSet
cDB ps = addPackages emptySet ps FromRepository
