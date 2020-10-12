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
  , setWithShowTime
  , ErrorLoggerIO, fromELM, toELM, execIO, putStrELM, putStrLnELM, runELM
  , (|>=), (|>)
  , mapEL
  , foldEL
  , succeedIO
  , failIO, failELM
  , log, logMsg
  , showLogEntry
  , infoMessage, debugMessage, errorMessage, fromErrorLogger
  , showExecCmd, execQuietCmd
  ) where

import Global
import IO      ( hPutStrLn, stderr )
import System  ( exitWith, system )

import Debug.Profile -- for show run-time
import Text.Pretty

infixl 0 |>=, |>

--- An error logger.
type ErrorLogger a = ([LogEntry], Either LogEntry a)

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

--- The global value for the log level.
logLevel :: Global LogLevel
logLevel = global Info Temporary

--- Gets the global log level. Messages below this level will not be printed.
getLogLevel :: IO LogLevel
getLogLevel = readGlobal logLevel

--- Sets the global log level. Messages below this level will not be printed.
setLogLevel :: LogLevel -> IO ()
setLogLevel level = writeGlobal logLevel level

-- Should the current time be shown with every log information?
withShowTime :: Global Bool
withShowTime = global False Temporary

--- Gets the "show time" information.
getWithShowTime :: IO Bool
getWithShowTime = readGlobal withShowTime

--- Sets the "show time" information. If true, then timing information
--- will be shown with every log information.
setWithShowTime :: Bool -> IO ()
setWithShowTime wst = writeGlobal withShowTime wst

---------------------------------------------------------------------------
--- Datatype to define the `ErrorLoggerIO` monad.
data ErrorLoggerIO a = ErrorLoggerIO (IO (ErrorLogger a))

--- Transforms an `ErrorLoggerIO` monad action into an IO action.
fromELM :: ErrorLoggerIO a -> IO (ErrorLogger a)
fromELM (ErrorLoggerIO errio) = errio

--- Transforms an IO action into an `ErrorLoggerIO` monad action.
toELM :: IO (ErrorLogger a) -> ErrorLoggerIO a
toELM act = ErrorLoggerIO act

--- Definition of the `ErrorLoggerIO` monad.
instance Monad ErrorLoggerIO where
  return = toELM  . succeedIO
  a >>= f = toELM (fromELM a |>= \x -> fromELM (f x))

--- Executes an IO action in the `ErrorLoggerIO` monad.
execIO :: IO a -> ErrorLoggerIO a
execIO act = toELM (act >>= succeedIO)

--- Prints a string in the `ErrorLoggerIO` monad.
putStrELM :: String -> ErrorLoggerIO ()
putStrELM = execIO . putStr

--- Prints a line in the `ErrorLoggerIO` monad.
putStrLnELM :: String -> ErrorLoggerIO ()
putStrLnELM = execIO . putStrLn

--- Runs an `ErrorLoggerIO` monad action as an IO action.
--- Shows all messages and exit with status 1 if an error occurred.
runELM :: ErrorLoggerIO a -> IO a
runELM elmact = do
  (msgs, result) <- fromELM elmact
  mapM showLogEntry msgs
  let allOk =  all (levelGte Info) (map logLevelOf msgs) &&
               either (\le -> levelGte Info (logLevelOf le))
                      (const True)
                      result
  unless allOk $ exitWith 1
  case result of Left  m -> showLogEntry m >> exitWith 1
                 Right v -> return v

----------------------------------------------------------------------------

-- Chains two actions passing the result from the first to the second.
(|>=) :: IO (ErrorLogger a) -> (a -> IO (ErrorLogger b)) -> IO (ErrorLogger b)
a |>= f = do
  (msgs, err) <- a 
  mapM showLogEntry msgs
  case err of
    Right v -> do (msgs', err') <- f v 
                  return (msgs', err')
    Left  m -> return ([], Left m)

--- Chains two actions ignoring the result of the first.
(|>) :: IO (ErrorLogger a) -> IO (ErrorLogger b) -> IO (ErrorLogger b)
a1 |> a2 = a1 |>= \_ -> a2

--- Maps an action over a list of values. Fails if one of the actions fails.
mapEL :: (a -> IO (ErrorLogger b)) -> [a] -> IO (ErrorLogger [b])
mapEL _ [] = succeedIO []
mapEL f (x:xs) = do
  (msgs, err) <- f x
  mapM showLogEntry msgs
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
  mapM showLogEntry msgs
  case err of
    Right v -> foldEL f v xs
    Left m -> return $ ([], Left m)

--- Renders a log entry to stderr.
showLogEntry :: LogEntry -> IO ()
showLogEntry (LogEntry lvl msg) = do
  minLevel <- getLogLevel
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

--- Create an action that always succeeds.
succeed :: a -> ErrorLogger a
succeed v = ([], Right v)

--- Create an IO action that always succeeds.
succeedIO :: a -> IO (ErrorLogger a)
succeedIO v = return $ succeed v

--- Create an action that always fails.
fail :: String -> ErrorLogger a
fail msg = ([logmsg], Left logmsg)
 where logmsg = LogEntry Critical msg

--- Create an IO action that always fails.
failIO :: String -> IO (ErrorLogger a)
failIO msg = return $ fail msg

--- Create an `ErrorLoggerIO` action that always fails with a message.
failELM :: String -> ErrorLoggerIO a
failELM msg = toELM (failIO msg)

--- Creates an IO action that logs a message.
logMsg :: LogLevel -> String -> ErrorLoggerIO ()
logMsg lvl msg = toELM $ log lvl msg

--- Creates an IO action that logs a message.
log :: LogLevel -> String -> IO (ErrorLogger ())
log lvl msg = do
  wst <- getWithShowTime
  if wst
    then do
      runtime <- getProcessInfos >>= return . maybe 0 id . lookup ElapsedTime
      return $ ([LogEntry lvl (showTime runtime ++ 's':' ':msg)], Right ())
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

--- Prints an error message in the standard IO monad.
errorMessage :: String -> IO ()
errorMessage msg = (log Error msg |> succeedIO ()) >> done

--- Transforms an error logger action into a standard IO action.
--- It shows all messages and, if the result is not available,
--- exits with a non-zero code.
fromErrorLogger :: IO (ErrorLogger a) -> IO a
fromErrorLogger a = do
  (msgs, err) <- a 
  mapM showLogEntry msgs
  case err of
    Left  m -> showLogEntry m >> exitWith 1
    Right v -> return v

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
