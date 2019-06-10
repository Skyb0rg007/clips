{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Definitions for the core CLIPS functions
module Clips.Raw.Core (
        -- * Creating and Destroying Environments
          createEnv
        , destroyEnv
        -- * Loading Constructs
        , clearEnv
        , loadEnvFromFile
        -- * Creating and Removing Facts and Instances
        , assertString
        , makeInstance
        , retractFact
        , unmakeInstance
        -- * Executing Rules
        , resetEnv
        , runEnv
        -- * Debugging
        , enableDribble
        , disableDribble
        , watchEnv
        , unwatchEnv
        ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

{# import        Clips.Raw.Types #}

#include "clips.h"

fromCEnum :: Enum a => a -> CInt
fromCEnum = fromIntegral . fromEnum

toCEnum :: Enum a => CInt -> a
toCEnum = toEnum . fromIntegral

-- | Create a new CLIPS environment
createEnv :: IO EnvironmentPtr
createEnv = {# call unsafe CreateEnvironment as ^ #}

-- | Destroy a CLIPS environment
destroyEnv :: EnvironmentPtr -> IO Bool
destroyEnv = fmap toBool . destroyEnvC

foreign import ccall unsafe "DestroyEnvironment"
    destroyEnvC :: EnvironmentPtr -> IO CBool

-- | Completely reset a CLIPS environment
clearEnv :: EnvironmentPtr -> IO Bool
clearEnv = fmap toBool . clearEnvC

foreign import ccall unsafe "Clear"
    clearEnvC :: EnvironmentPtr -> IO CBool

-- | Load the environment from the given file
loadEnvFromFile :: EnvironmentPtr -> FilePath -> IO LoadError
loadEnvFromFile env path = fmap toCEnum . withCString path $
    {# call unsafe Load as ^ #} env

-- | Assert a fact from a string
-- Returns a pointer to the fact,
-- which is only valid until the next call, unless retained
assertString :: EnvironmentPtr
             -> String
             -> IO (Either AssertStringError FactPtr)
assertString env str = do
    factPtr <- withCString str $ {# call unsafe AssertString as aStr #} env
    let errorCode = toCEnum <$> {# call GetAssertStringError as ^ #} env
    if factPtr == nullPtr
        then Left <$> errorCode
        else pure $ Right factPtr

-- | Create an instance from a string
makeInstance :: EnvironmentPtr
             -> String
             -> IO (Either MakeInstanceError InstancePtr)
makeInstance env str = do
    instPtr <- withCString str $ {# call unsafe MakeInstance as mkInst #} env
    let errorCode = toCEnum <$> {# call GetMakeInstanceError as ^ #} env
    if instPtr == nullPtr
        then Left <$> errorCode
        else pure $ Right instPtr

-- | Retract a previously defined fact
retractFact :: FactPtr -> IO RetractError
retractFact = fmap toCEnum . {# call unsafe Retract as ^ #}

-- | Remove a previously instantiated class member
unmakeInstance :: InstancePtr -> IO UnmakeInstanceError
unmakeInstance = fmap toCEnum . {# call unsafe UnmakeInstance as unmkInst #}

-- | Reset an environment, leaving deffacts, defrules, definstances, etc.
resetEnv :: EnvironmentPtr -> IO ()
resetEnv = {# call unsafe Reset as resetEnvC #}

-- | Run an environment, at most n times. passing (-1) means infinitely
runEnv :: EnvironmentPtr -> Int -> IO Int
runEnv env n = fromIntegral
    <$> {# call unsafe Run as runEnvC #} env (fromIntegral n)

-- | Enable dribbling to a file
enableDribble :: EnvironmentPtr -> FilePath -> IO Bool
enableDribble env path = fmap toBool . withCString path $ enableDribbleC env

foreign import ccall unsafe "DribbleOn"
    enableDribbleC :: EnvironmentPtr -> CString -> IO CBool

-- | Disable dribbling to a file
disableDribble :: EnvironmentPtr -> IO Bool
disableDribble = fmap toBool . disableDribbleC

foreign import ccall unsafe "DribbleOff"
    disableDribbleC :: EnvironmentPtr -> IO CBool

-- | Watch a specific type of expression
watchEnv :: EnvironmentPtr -> WatchItem -> IO ()
watchEnv env wi = {# call unsafe Watch as ^ #} env $ fromCEnum wi

-- | Remove a watch on a type of expression
unwatchEnv :: EnvironmentPtr -> WatchItem -> IO ()
unwatchEnv env wi = {# call unsafe Unwatch as ^ #} env $ fromCEnum wi
