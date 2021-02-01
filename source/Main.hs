{-# LANGUAGE MultiWayIf, LambdaCase #-}

module Main where

import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Exception
import           Data.List
import           System.Directory
import           System.Console.Haskeline
import           Text.ParserCombinators.Parsec hiding (try)
import           Options.Applicative
import           Data.List.Split (splitOn)
import           Format
import           Interpreter
import           Environment
    


-- Lambda interpreter.
-- The actions of the interpreter are written here. It allows to execute normal
-- actions (bindings and evaluation), and interpreter specific actions, as "quit"
-- or "load".


-- | Runs the interpreter with default settings and an empty context.
main :: IO ()
main = do
  let parsedOptions = info 
        (helper <*> options) 
        (  fullDesc
        <> header ("Mikrokosmos Lambda Interpreter, ver. " ++ version)
        )
  opts <- execParser parsedOptions
  let 
    initialEnv  = flip changePlain (flagPlain opts) 
                . flip changeSilent (flagSilent opts) 
                . flip changeColor (not $ flagColor opts)       
                $ if flagLibs opts then defaultEnv else librariesEnv    
    filename = flagExec opts
  -- Reads the rest of the flags. The --version flag shows the current
  -- version of the interpreter. An optional argument may be used to
  -- indicate a file to load.
  if  | flagVersion opts -> putStrLn versionText
      | null filename    -> runInputT
          defaultSettings
          (outputStrLn ((if getColor initialEnv then id else decolor) initialText)
              >> interpreterLoop initialEnv)
      | otherwise        -> 
          foldM_ executeFile initialEnv $ splitOn "," filename

-- | Interpreter awaiting for an instruction.
interpreterLoop :: Environment -> InputT IO ()
interpreterLoop environment = do
  -- Gets the user input on the interpreter
  -- and parses it to a concrete action.
  minput <- getInputLine ((if getColor environment then id else decolor) promptText)
  let interpreteraction =
        case minput of
          Nothing -> Quit
          Just "" -> Interpret EmptyLine
          Just input ->
            case parse interpreteractionParser "" (preformat input) of
              Left  _ -> Interpret Error
              Right a -> a

  newenvironment <- executeAction environment interpreteraction
  forM_ newenvironment interpreterLoop
              

-- | Executes the parsed action, every action may affect the context
-- in a way, and returns the control to the interpreter.
executeAction :: Environment -> InterpreterAction -> InputT IO (Maybe Environment)
executeAction environment interpreteraction = 
  case interpreteraction of
    -- Interprets an action
    Interpret actn ->
      case runState (act actn) environment of
        (output, newenv) -> do
          outputActions newenv output
          return $ Just newenv
    -- Loads a module and its dependencies given its name.
    -- Avoids repeated modules keeping only their first ocurrence.
    Load modulename -> do
      readallmoduledeps <- lift $ readAllModuleDepsRecursively [modulename]
      case readallmoduledeps of
        Nothing -> do
          outputStrLn errorNotFoundText
          return $ Just environment
        Just readallmodules -> do
          let modules = nub readallmodules
          files <- lift $ mapM findFilename modules
          -- Concats all the module contents
          case sequence files of
            Nothing -> do
              outputStrLn errorNotFoundText
              return $ Just environment
            Just allfiles -> do
              maybeactions <- fmap concat . sequence <$> lift (mapM (loadFile environment) allfiles)
              case maybeactions of
                Nothing -> do
                  outputStrLn "Error loading file"
                  return $ Just environment
                Just actions ->
                  case runState (multipleAct actions) environment of
                    (output, newenv) -> do
                      outputActions newenv output
                      return $ Just newenv
    -- Exits the interpreter
    Quit -> return Nothing

-- | Outputs results from actions. Given a list of options and outputs,
--   formats and prints them in console.
outputActions :: Environment -> [String] -> InputT IO ()
outputActions environment output = do
    outputStr (if getColor environment then formatFormula else "")
    mapM_ (outputStr . format) output
    outputStr end
  where
    format = (if getPlain environment then formatPlain else id) . formatColor
    formatColor s 
      | getColor environment = s
      | otherwise            = unlines $ map decolor $ lines s
    formatPlain = 
      concatMap (\ch -> case ch of
        'λ' -> "\\"
        '⇒' -> "=>"
        _ -> [ch]) 

-- Loading and reading files
-- | Loads the given filename and returns the complete list of actions.
--   Returns Nothing if there is an error reading or parsing the file.
loadFile :: Environment -> Filename -> IO (Maybe [Action])
loadFile env filename = do
  unless (getSilent env) $ putStrLn $ 
    (if getColor env then formatLoading else "") ++ "Loading " ++ filename ++ "..." ++ end
  input <- try $ readFile filename :: IO (Either IOException String)
  case input of
    Left _ -> return Nothing
    Right inputs -> do
      let parsing = map (parse actionParser "" . preformat) . filter (/="") . lines $ inputs
      let actions = map (\case 
                            Left _  -> Nothing
                            Right a -> Just a) parsing
      return $ sequence actions

-- | Executes the commands inside a file. A .mkr file can contain a sequence of
--   expressions and variable bindings, and it is interpreted sequentially.
executeFile :: Environment -> Filename -> IO Environment
executeFile env filename = do
  maybeloadfile <- loadFile env filename
  case maybeloadfile of
    Nothing -> do
      putStrLn "Error loading file"
      return env
    Just actions -> do
      let (outputs, newenv) = runState (multipleAct actions) env
      runInputT defaultSettings $ outputActions newenv outputs
      return newenv

-- | Reads module dependencies
readFileDependencies :: Filename -> IO [Modulename]
readFileDependencies filename = do
  input <- try $ readFile filename :: IO (Either IOException String)
  case input of
    Left _ -> return []
    Right inputs -> return $
      map (drop 9) (filter (isPrefixOf "#INCLUDE ") $ filter (/="") $ lines inputs)

-- | Reads all the dependencies from a module list.
--   Returns an error if a dependency cannot be found
readAllModuleDeps :: [Modulename] -> IO (Maybe [Modulename])
readAllModuleDeps modulenames = do
  files <- mapM findFilename modulenames
  deps <- mapM (mapM readFileDependencies) files
  return (concat <$> sequence deps)

-- | Read module dependencies recursively.
--   Returns an error if a dependency cannot be found
readAllModuleDepsRecursively :: [Modulename] -> IO (Maybe [Modulename])
readAllModuleDepsRecursively modulenames = do
  maybenewmodulenames <- readAllModuleDeps modulenames
  case maybenewmodulenames of
    Nothing -> return Nothing
    Just newmodulenames -> do
      let allmodulenames = nub (newmodulenames ++ modulenames)
      if modulenames == allmodulenames
      then return (Just modulenames)
      else readAllModuleDepsRecursively allmodulenames

-- | Given a module name, returns the filename associated with it
findFilename :: Modulename -> IO (Maybe Filename)
findFilename s = do
  appdir <- getAppUserDataDirectory "mikrokosmos"
  homedir <- getHomeDirectory

  -- Looks for the module in the common locations
  headMaybe <$> filterM doesFileExist
    [ "lib/" ++ s ++ ".mkr"
    , "./" ++ s ++ ".mkr"
    , appdir ++ "/" ++ s ++ ".mkr"
    , homedir ++ "/" ++ s ++ ".mkr"
    , "/usr/lib/mikrokosmos/" ++ s ++ ".mkr"
    ]
  where
    headMaybe [] = Nothing
    headMaybe (x:_) = Just x


-- | Flags datatype
data MainFlags = MainFlags
  { flagExec :: String
  , flagVersion :: Bool
  , flagLibs :: Bool
  , flagPlain :: Bool 
  , flagSilent :: Bool
  , flagColor :: Bool
  }
  deriving (Show)
-- !!!!  

-- | Flags definition
options :: Options.Applicative.Parser MainFlags
options = MainFlags
    <$> strOption    
      (  long "exec" 
      <> short 'e'    
      <> value ""    
      <> help "A file (files) to execute and show its results"
      <> metavar "file[,file...]"
      )
    <*> switch
      (  long "version" 
      <> short 'v'
      <> help "Show program version"
      )
    <*> switch
      (  long "no-libs" 
      <> short 'l' 
      <> help "Runs mikrokosmos without standard libraries"
      )
    <*> switch
      (  long  "plain"   
      <> short 'p'
      <> help "Do not use Unicode symbols for output"
      )
    <*> switch
      (  long "silent"  
      <> short 's' 
      <> help "For batch mode print only computation outputs and error diagnostics"
      )
    <*> switch
      (  long "no-color"  
      <> short 'c' 
      <> help "Prohibit output colorization"
      )
