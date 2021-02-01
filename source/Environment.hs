{-|
Module: Environment
Description: Internal state and environment of the interpreter
License: GPL-3

This module contains all the auxiliary logic necessary to represent the internal
state of the interpreter.
-}
module Environment
  (
  -- * Environment
    Environment
  , context
  , defaultEnv
  , emptyContext

  -- * Reading the environment
  , getVerbose
  , getColor
  , getPlain
  , getSilent
  , getSki
  , getTypes
  , getExpressionName
  , getStrategy
  , getTopo

  -- * Modifying the environment
  , addBind
  , changeColor
  , changeVerbose
  , changeSkioutput
  , changeTypes
  , changeStrategy
  , changeTopo
  , changePlain
  , changeSilent
  

  -- * Filenames and Modulenames
  , Filename
  , Modulename
  )
where

import           Data.List
import           MultiBimap
import           Lambda
-- | A filename is a string containing the directory path and
-- the real name of the file.
type Filename = String

-- | A modulename is a string naming a module.
type Modulename = String


data Environment = Environment
  { context :: Context
  , loadedFiles :: [Filename]
  , verbose :: Bool
  , color :: Bool
  , plain :: Bool
  , silent :: Bool 
  , skioutput :: Bool
  , types :: Bool
  , strategy :: String
  , topo :: Bool
  }
  
-- !!!!
instance Show Environment where
  show e  = "Environment { " ++
            "loadedFiles = " ++ show (loadedFiles e) ++ ", " ++
            "verbose = " ++ show (verbose e) ++ ", " ++
            "color = " ++ show (color e) ++ ", " ++
            "plain = " ++ show (plain e) ++ ", " ++
            "silent = " ++ show (silent e) ++ ", " ++
            "skioutput = " ++ show (skioutput e) ++ ", " ++
            "types = " ++ show (types e) ++ ", " ++
            "strategy = " ++ show (strategy e) ++ ", " ++
            "topo = " ++ show (topo e) ++ " }"
-- !!!!

-- | Default environment for the interpreter.
defaultEnv :: Environment
defaultEnv = Environment
  { context     = emptyContext
  , loadedFiles = []
  , verbose     = False
  , color       = True
  , plain       = False
  , silent      = False
  , skioutput   = False
  , types       = False
  , strategy    = "full"
  , topo        = False
  }

-- | Get current settings
getColor, getVerbose, getSki, getTypes, getTopo, getPlain, getSilent :: Environment -> Bool
getColor    = color
getPlain    = plain
getSilent   = silent
getVerbose  = verbose
getSki      = skioutput
getTypes    = types
getTopo     = topo

getStrategy :: Environment -> String
getStrategy = strategy


-- | Adds a name binding to the environment
addBind :: Environment -> String -> Exp -> Environment
addBind env s e =
  -- If the binding already exists, it changes nothing
  if s `elem` MultiBimap.lookup e (context env)
    then env
    else env {context = MultiBimap.insert e s (context env)}

-- | Sets the verbose configuration on/off.
changeVerbose :: Environment -> Bool -> Environment
changeVerbose options setting = options {verbose = setting}

-- | Sets the color configuration on/off
changeColor :: Environment -> Bool -> Environment
changeColor options setting = options {color = setting}

-- | Sets the ski output configuration on/off
changeSkioutput :: Environment -> Bool -> Environment
changeSkioutput options setting = options {skioutput = setting}

-- | Sets the types output configuration on/off
changeTypes :: Environment -> Bool -> Environment
changeTypes options setting = options {types = setting}

-- | Sets the reduction strategy 
changeStrategy :: Environment -> String -> Environment
changeStrategy options setting = options {strategy = setting}

changeTopo :: Environment -> Bool -> Environment
changeTopo options setting = options {topo = setting}

changePlain :: Environment -> Bool -> Environment
changePlain env setting = env { plain = setting }

changeSilent :: Environment -> Bool -> Environment
changeSilent env setting = env { silent = setting }



-- | Given an expression, returns its name if it is bounded to any.
getExpressionName :: Environment -> Exp -> Maybe String
getExpressionName environment expr = case MultiBimap.lookup expr (context environment) of
  [] -> Nothing
  xs -> Just $ intercalate ", " xs



-- | A context is an application between expressions and the names
-- they may have.
type Context  = MultiBimap Exp String

-- | Empty context without any bindings
emptyContext :: Context
emptyContext = MultiBimap.empty
