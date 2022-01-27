--------------------------------------------------------------------------------
--- Contains combinators for chaining IO actions that can fail and log messages.
--------------------------------------------------------------------------------

module CPM.ErrorLogger
  ( ErrorLogger (runErrorLogger)
  , LogEntry
  , LogLevel (..), logLevelOf
  , logAt, getLogLevel, setLogLevel, getWithShowTime, setWithShowTime
  , logInfo, logDebug, logError, logCritical, showLogEntry, levelGte
  , putStrELM, putStrLnELM
  , fromErrorLogger
  , showExecCmd, execQuietCmd, liftIOEL, tryEL
  , inDirectoryEL, inTempDirEL
  ) where

import Control.Monad    ( unless )
import System.IO        ( hPutStrLn, stderr )
import System.Process   ( exitWith, system )
import System.Directory

import CPM.FileUtil

import Debug.Profile -- for show run-time
import Text.Pretty hiding (empty)

-- A value or an error, along with logs.
type ErrorLog a = ([LogEntry], Either LogEntry a)

--- An error logging IO monad.
newtype ErrorLogger a = ErrorLogger
  {
    runErrorLogger :: LogLevel -> Bool
                   -> IO ((LogLevel, Bool), ErrorLog a)
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
    Quiet    -> text "QUIET "  -- should not occur...
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

--- Logs a message at a user-defined level.
logAt :: LogLevel -> String -> ErrorLogger ()
logAt lvl msg = ErrorLogger $ \l wst ->
  if wst
    then do
      runtime <- getProcessInfos >>= return . maybe 0 id . lookup ElapsedTime
      return ((l, wst), ([LogEntry lvl (showTime runtime ++ 's':' ':msg)], Right ()))
    else
      return ((l, wst), ([LogEntry lvl msg], Right ()))
 where
  showTime t = show (t `div` 1000) ++ "." ++ show2 ((t `mod` 1000) `div` 10)
  show2 i = if i < 10 then '0' : show i else show i

--- Logs a message at the info level.
logInfo :: String -> ErrorLogger ()
logInfo = logAt Info

--- Logs a message at the debug level.
logDebug :: String -> ErrorLogger ()
logDebug = logAt Debug

--- Logs a message at the error level.
logError :: String -> ErrorLogger ()
logError = logAt Error

--- Logs a message at the critical level.
logCritical :: String -> ErrorLogger ()
logCritical = logAt Critical

--- Prints a string in the `ErrorLogger` monad.
putStrELM :: String -> ErrorLogger ()
putStrELM = liftIOEL . putStr

--- Prints a line in the `ErrorLogger` monad.
putStrLnELM :: String -> ErrorLogger ()
putStrLnELM = liftIOEL . putStrLn

--- Transforms an error logger action into a standard IO action.
--- It shows all messages and, if the result is not available,
--- exits with a non-zero code.
--- The first argument specifies the logging level for messages.
--- If the second argument is true, timings are shown in the messages.
fromErrorLogger :: LogLevel -> Bool -> ErrorLogger a -> IO a
fromErrorLogger l s a = do
  ((glob, _), (msgs, err)) <- runErrorLogger a l s
  mapM (showLogEntry glob) msgs
  case err of
    Right v -> return v
    Left  m -> showLogEntry glob m >> exitWith 1

--- Returns the current log level.
getLogLevel :: ErrorLogger LogLevel
getLogLevel = ErrorLogger $ \ l s -> return ((l, s), ([], Right l))

--- Sets the current log level.
setLogLevel :: LogLevel -> ErrorLogger ()
setLogLevel l = ErrorLogger $ \ _ s -> return ((l, s), ([], Right ()))

--- Returns the current time-showing mode.
getWithShowTime :: ErrorLogger Bool
getWithShowTime = ErrorLogger $ \ l s -> return ((l, s), ([], Right s))

--- Sets the current time-showing mode.
setWithShowTime :: Bool -> ErrorLogger ()
setWithShowTime s = ErrorLogger $ \ l _ -> return ((l, s), ([], Right ()))

--- Lifts an IO action into the `ErrorLogger` monad.
liftIOEL :: IO a -> ErrorLogger a
liftIOEL ma = ErrorLogger (\l s -> do a <- ma
                                      return ((l, s), ([], Right a)))

--- Tries to execute an EL action and returns either an error that
--- occurred or the value. If the first argument is `True`, the execution
--- will run quietly.
tryEL :: Bool -> ErrorLogger a -> ErrorLogger (Either LogEntry a)
tryEL quiet a = do
  ll    <- if quiet then return Quiet else getLogLevel
  stime <- getWithShowTime
  liftIOEL $ fmap (snd . snd) $ runErrorLogger a ll stime

--- Executes a system command and shows the command as debug message.
showExecCmd :: String -> ErrorLogger Int
showExecCmd cmd = do
  logDebug $ "Executing: " ++ cmd
  ec <- liftIOEL $ system cmd
  unless (ec == 0) $
    logDebug $ "Command terminated with non-zero exit code " ++ show ec
  return ec

--- Executes a command depending on the verbosity mode.
--- If the log level is not `Debug`, the first (quiet) command is executed,
--- otherwise the second (more verbose) command is executed.
execQuietCmd :: String -> String -> ErrorLogger Int
execQuietCmd quietcmd verbosecmd = do
  l <- getLogLevel
  showExecCmd $ if l == Debug then verbosecmd else quietcmd

--- Executes an EL action with the current directory set to a specific
--- directory.
inDirectoryEL :: String -> ErrorLogger b -> ErrorLogger b
inDirectoryEL dir b = do
  previous <- liftIOEL getCurrentDirectory
  logDebug $ "Set current working directory to: " ++ dir
  liftIOEL $ setCurrentDirectory dir
  b' <- b
  logDebug $ "Set current working directory to: " ++ previous
  liftIOEL $ setCurrentDirectory previous
  return b'

--- Executes an EL action with the current directory set to  CPM's temporary
--- directory.
inTempDirEL :: ErrorLogger b -> ErrorLogger b
inTempDirEL b = do
  t <- liftIOEL tempDir
  exists <- liftIOEL $ doesDirectoryExist t
  if exists
    then return ()
    else liftIOEL $ createDirectory t
  inDirectoryEL t b
