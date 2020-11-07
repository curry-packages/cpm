--------------------------------------------------------------------------------
--- Contains combinators for chaining IO actions that can fail and log messages.
--------------------------------------------------------------------------------

module CPM.ErrorLogger
  ( ErrorLogger (runErrorLogger)
  , LogEntry
  , LogLevel (..), logLevelOf
  , log, getLogLevel, setLogLevel, getWithShowTime, setWithShowTime
  , infoMessage, debugMessage, errorMessage, criticalMessage, showLogEntry, levelGte
  , putStrELM, putStrLnELM
  , fromErrorLogger
  , showExecCmd, execQuietCmd, liftIOErrorLogger
  , inDirectoryEL, inTempDirEL
  ) where

import System.IO      ( hPutStrLn, stderr )
import System.Process ( exitWith, system )
import System.Directory

import CPM.FileUtil

import Debug.Profile -- for show run-time
import Text.Pretty hiding (empty)

--- An error logger.
newtype ErrorLogger a = ErrorLogger
  {
    runErrorLogger :: LogLevel -> Bool
                   -> IO ((LogLevel, Bool), ([LogEntry], Either LogEntry a))
  }

--- A log entry.
data LogEntry = LogEntry LogLevel String

logLevelOf :: LogEntry -> LogLevel
logLevelOf (LogEntry ll _) = ll

--- A log level.
data LogLevel = Quiet
              | Info
              | Debug
              | Error
              | Critical
 deriving Eq

---------------------------------------------------------------------------

instance Functor ErrorLogger where
  fmap f e = ErrorLogger $ \l s -> do
    (st, (msgs, err)) <- runErrorLogger e l s
    let (glob, _) = st
    mapM (showLogEntry glob) msgs
    case err of
      Left  v -> return (st, ([], Left v))
      Right a -> return (st, ([], Right (f a)))

instance Applicative ErrorLogger where
  pure = return
  f <*> v = f >>= \f' -> fmap f' v

instance Alternative ErrorLogger where
  empty = fail "empty"
  a <|> b = ErrorLogger $ \l s -> do
    (st, (msgs, a')) <- runErrorLogger a l s
    let (glob, showTime) = st
    mapM (showLogEntry glob) msgs
    case a' of
      Left  _ -> runErrorLogger b glob showTime
      Right v -> return (st, ([], Right v))

instance Monad ErrorLogger where
  return a = ErrorLogger $ \l s -> return ((l, s), ([], Right a))
  m >>= f = ErrorLogger $ \l s -> do
    (st, (msgs, err)) <- runErrorLogger m l s
    let (glob, showTime) = st
    mapM (showLogEntry glob) msgs
    case err of
      Right v -> do
        (st', (msgs', err')) <- runErrorLogger (f v) glob showTime
        return $ (st', (msgs', err'))
      Left  e -> return $ (st, ([], Left e))

instance MonadFail ErrorLogger where
  fail msg = ErrorLogger $ \l s -> return ((l, s), ([logMsg], Left logMsg))
    where logMsg = LogEntry Critical msg

--- Renders a log entry to stderr.
showLogEntry :: LogLevel -> LogEntry -> IO ()
showLogEntry minLevel (LogEntry lvl msg) = do
  if levelGte lvl minLevel
    then mapM_ (\l -> hPutStrLn stderr $ pPrint $ lvlText <+> text l)
               (lines msg)
    else return ()
 where
  lvlText = case lvl of
    Quiet    -> text "QUIET "  -- show not occur...
    Info     -> text "INFO "
    Debug    -> green $ text "DEBUG "
    Critical -> red   $ text "CRITICAL "
    Error    -> red   $ text "ERROR "

--- Compares two log levels.
levelGte :: LogLevel -> LogLevel -> Bool
levelGte Debug Debug    = True
levelGte Debug Quiet    = False
levelGte Debug Info     = False
levelGte Debug Error    = False
levelGte Debug Critical = False
levelGte Info  Debug    = True
levelGte Info  Info     = True
levelGte Info  Quiet    = False
levelGte Info  Error    = False
levelGte Info  Critical = False
levelGte Quiet Debug    = True
levelGte Quiet Quiet    = True
levelGte Quiet Info     = False
levelGte Quiet Error    = False
levelGte Quiet Critical = False
levelGte Error Debug    = True
levelGte Error Info     = True
levelGte Error Quiet    = True
levelGte Error Error    = True
levelGte Error Critical = True
levelGte Critical Debug = True
levelGte Critical Info  = True
levelGte Critical Quiet = True
levelGte Critical Error = True
levelGte Critical Critical = True

log :: LogLevel -> String -> ErrorLogger ()
log lvl msg = ErrorLogger $ \l wst ->
  if wst
    then do
      runtime <- getProcessInfos >>= return . maybe 0 id . lookup ElapsedTime
      return ((l, wst), ([LogEntry lvl (showTime runtime ++ 's':' ':msg)], Right ()))
    else
      return ((l, wst), ([LogEntry lvl msg], Right ()))
 where
  showTime t = show (t `div` 1000) ++ "." ++ show ((t `mod` 1000) `div` 10)

infoMessage :: String -> ErrorLogger ()
infoMessage msg = log Info msg

debugMessage :: String -> ErrorLogger ()
debugMessage msg = log Debug msg

errorMessage :: String -> ErrorLogger ()
errorMessage msg = log Error msg

criticalMessage :: String -> ErrorLogger ()
criticalMessage msg = log Critical msg

--- Prints a string in the `ErrorLogger` monad.
putStrELM :: String -> ErrorLogger ()
putStrELM = execIO . putStr

--- Prints a line in the `ErrorLogger` monad.
putStrLnELM :: String -> ErrorLogger ()
putStrLnELM = execIO . putStrLn

--- Transforms an error logger action into a standard IO action.
--- It shows all messages and, if the result is not available,
--- exits with a non-zero code.
fromErrorLogger :: LogLevel -> Bool -> ErrorLogger a -> IO a
fromErrorLogger l s a = do
  ((glob, _), (msgs, err)) <- runErrorLogger a l s
  mapM (showLogEntry glob) msgs
  case err of
    Right v -> return v
    Left  m -> showLogEntry glob m >> exitWith 1

--- Executes a system command and show the command as debug message.
showExecCmd :: String -> ErrorLogger Int
showExecCmd cmd = debugMessage ("Executing: " ++ cmd) >>
  liftIOErrorLogger (system cmd)

--- Executes a parameterized system command.
--- The parameter is set to `-q` unless the LogLevel is Debug.
execQuietCmd :: (String -> String) -> ErrorLogger Int
execQuietCmd cmd = debugMessage ("Executing: " ++ cmd "") >>
  ErrorLogger (\l s -> do i <- system $ cmd (if l == Debug then "" else "-q")
                          return ((l, s), ([], Right i)))

getLogLevel :: ErrorLogger LogLevel
getLogLevel = ErrorLogger $ \ l s -> return ((l, s), ([], Right l))

getWithShowTime :: ErrorLogger Bool
getWithShowTime = ErrorLogger $ \ l s -> return ((l, s), ([], Right s))

setLogLevel :: LogLevel -> ErrorLogger ()
setLogLevel l = ErrorLogger $ \ _ s -> return ((l, s), ([], Right ()))

setWithShowTime :: Bool -> ErrorLogger ()
setWithShowTime s = ErrorLogger $ \ l _ -> return ((l, s), ([], Right ()))

liftIOErrorLogger :: IO a -> ErrorLogger a
liftIOErrorLogger ma = ErrorLogger (\l s -> do a <- ma
                                               return ((l, s), ([], Right a)))

--- Executes an EL action with the current directory set to a specific
--- directory.
inDirectoryEL :: String -> ErrorLogger b -> ErrorLogger b
inDirectoryEL dir b = do
  previous <- liftIOErrorLogger getCurrentDirectory
  liftIOErrorLogger $ setCurrentDirectory dir
  b' <- b
  liftIOErrorLogger $ setCurrentDirectory previous
  return b'


--- Executes an EL action with the current directory set to  CPM's temporary
--- directory.
inTempDirEL :: ErrorLogger b -> ErrorLogger b
inTempDirEL b = do
  t <- liftIOErrorLogger tempDir
  exists <- liftIOErrorLogger $ doesDirectoryExist t
  if exists
    then return ()
    else liftIOErrorLogger $ createDirectory t
  inDirectoryEL t b
