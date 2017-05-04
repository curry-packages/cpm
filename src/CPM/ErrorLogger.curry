--------------------------------------------------------------------------------
--- Contains combinators for chaining IO actions that can fail and log messages.
--------------------------------------------------------------------------------

module CPM.ErrorLogger 
  ( ErrorLogger
  , LogEntry
  , LogLevel (..)
  , logLevelOf
  , levelGte
  , getLogLevel, setLogLevel
  , (|>=)
  , (|>)
  , (|->)
  , mapEL
  , foldEL
  , succeedIO
  , failIO
  , log
  , showLogEntry
  , infoMessage, debugMessage, fromErrorLogger
  , showExecCmd, execQuietCmd
  ) where

import Global
import Pretty
import Profile -- for show run-time
import System (exitWith, system)

infixl 0 |>=
infixl 0 |>

-- Should the current time be shown with every log information?
withShowTime :: Bool
withShowTime = False

--- An error logger.
type ErrorLogger a = ([LogEntry], Either LogEntry a)

--- A log entry.
data LogEntry = LogEntry LogLevel String

logLevelOf :: LogEntry -> LogLevel
logLevelOf (LogEntry ll _) = ll

--- A log level.
data LogLevel = Info
              | Debug
              | Error
              | Critical

--- The global value for the log level.
logLevel :: Global LogLevel
logLevel = global Info Temporary

--- Gets the global log level. Messages below this level will not be printed.
getLogLevel :: IO LogLevel
getLogLevel = readGlobal logLevel

--- Sets the global log level. Messages below this level will not be printed.
setLogLevel :: LogLevel -> IO ()
setLogLevel level = writeGlobal logLevel level

--- Chains two actions passing the result from the first to the second.
(|>=) :: IO (ErrorLogger a) -> (a -> IO (ErrorLogger b)) -> IO (ErrorLogger b)
a |>= f = do
  (msgs, err) <- a 
  mapIO showLogEntry msgs
  case err of
    Right v -> do
      (msgs', err') <- f v 
      return $ (msgs', err')
    Left  m -> return $ ([], Left m)

--- Chains two actions ignoring the result of the first.
(|>) :: IO (ErrorLogger a) -> IO (ErrorLogger b) -> IO (ErrorLogger b)
a |> f = do
  (msgs, err) <- a
  mapIO showLogEntry msgs
  case err of
    Right _ -> do
      (msgs', err') <- f
      return $ (msgs', err')
    Left m -> return $ ([], Left m)

--- Chains two actions ignoring the result of the second.
(|->) :: IO (ErrorLogger a) -> IO (ErrorLogger b) -> IO (ErrorLogger a)
a |-> b = do
  (msgs, err) <- a
  mapIO showLogEntry msgs
  case err of
    Right _ -> do
      (msgs', _) <- b
      return $ (msgs', err)
    Left m -> return $ ([], Left m)

--- Maps an action over a list of values. Fails if one of the actions fails.
mapEL :: (a -> IO (ErrorLogger b)) -> [a] -> IO (ErrorLogger [b])
mapEL _ [] = succeedIO []
mapEL f (x:xs) = do
  (msgs, err) <- f x
  mapIO showLogEntry msgs
  case err of
    Right v -> do
      (msgs', xs') <- mapEL f xs
      case xs' of
        Right xs'' -> succeedIO (v:xs'')
        Left    m'  -> return $ (msgs', Left m')
    Left m -> return $ ([], Left m)

--- Folds a list of values using an action. Fails if one of the actions fails.
foldEL :: (a -> b -> IO (ErrorLogger a)) -> a -> [b] -> IO (ErrorLogger a)
foldEL _ z [] = succeedIO z
foldEL f z (x:xs) = do
  (msgs, err) <- f z x
  mapIO showLogEntry msgs
  case err of
    Right v -> foldEL f v xs
    Left m -> return $ ([], Left m)

--- Renders a log entry.
showLogEntry :: LogEntry -> IO ()
showLogEntry (LogEntry lvl msg) = do
  minLevel <- getLogLevel
  if levelGte lvl minLevel
    then putStrLn $ pPrint $ lvlText <+> (text msg)
    else return ()
 where
  lvlText = case lvl of
    Info     -> text "INFO "
    Debug    -> green $ text "DEBUG "
    Critical -> red   $ text "CRITICAL "
    Error    -> red   $ text "ERROR "

--- Compares two log levels.
levelGte :: LogLevel -> LogLevel -> Bool
levelGte Debug Debug    = True
levelGte Debug Info     = False
levelGte Debug Error    = False
levelGte Debug Critical = False
levelGte Info  Debug    = True
levelGte Info  Info     = True
levelGte Info  Error    = False
levelGte Info  Critical = False
levelGte Error Debug    = True
levelGte Error Info     = True
levelGte Error Error    = True
levelGte Error Critical = True
levelGte Critical Debug = True
levelGte Critical Info  = True
levelGte Critical Error = True
levelGte Critical Critical = True

--- Create an action that always succeeds.
succeed :: a -> ErrorLogger a
succeed v = ([], Right v)

--- Create an IO action that always succeeds.
succeedIO :: a -> IO (ErrorLogger a)
succeedIO v = return $ succeed v

--- Create an action that always fails.
fail :: String -> ErrorLogger a
fail msg = ([logMsg], Left logMsg) where logMsg = LogEntry Critical msg

--- Create an IO action that always fails.
failIO :: String -> IO (ErrorLogger a)
failIO msg = return $ fail msg

--- Create an IO action that logs a message.
log :: LogLevel -> String -> IO (ErrorLogger ())
log lvl msg =
  if withShowTime
    then do
      runtime <- getProcessInfos >>= return . maybe 0 id . lookup ElapsedTime
      return $ ([LogEntry lvl (showTime runtime ++ ' ':msg)], Right ())
    else
      return $ ([LogEntry lvl msg], Right ())
 where
  showTime t = show (t `div` 1000) ++ "." ++ show ((t `mod` 1000) `div` 10)

--- Prints an info message in the standard IO monad.
infoMessage :: String -> IO ()
infoMessage msg = (log Info msg |> succeedIO ()) >> done

--- Prints a debug message in the standard IO monad.
debugMessage :: String -> IO ()
debugMessage msg = (log Debug msg |> succeedIO ()) >> done

--- Transforms an error logger actions into a standard IO action.
--- It shows all messages and, if the result is not available,
--- exits with a non-zero code.
fromErrorLogger :: IO (ErrorLogger a) -> IO a
fromErrorLogger a = do
  (msgs, err) <- a 
  mapIO showLogEntry msgs
  case err of
    Right v -> return v
    Left  m -> showLogEntry m >> exitWith 1

--- Executes a system command and show the command as debug message.
showExecCmd :: String -> IO Int
showExecCmd cmd = debugMessage ("Executing: " ++ cmd) >> system cmd

--- Executes a parameterized system command.
--- The parameter is set to `-q` unless the LogLevel is Debug.
execQuietCmd :: (String -> String) -> IO Int
execQuietCmd cmd = do
  ll <- getLogLevel
  debugMessage $ "Executing: " ++ cmd ""
  system $ cmd (if ll == Debug then "" else "-q")
