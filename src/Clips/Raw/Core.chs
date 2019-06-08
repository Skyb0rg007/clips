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
        , getAssertStringError
        , makeInstance
        , getMakeInstanceError
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

-- | Create a new CLIPS environment
foreign import ccall unsafe "CreateEnvironment"
    createEnv :: IO EnvironmentPtr

-- | Destroy a CLIPS environment
foreign import ccall unsafe "DestroyEnvironment"
    destroyEnv :: EnvironmentPtr -> IO Bool

-- | Completely reset a CLIPS environment
foreign import ccall unsafe "Clear"
    clearEnv :: EnvironmentPtr -> IO Bool

-- | Load the environment from the given file
{# fun unsafe Load as loadEnvFromFile
    {`EnvironmentPtr', `CString'} -> `LoadError' #}

-- | Assert a fact from a string
-- Returns a pointer to the fact,
-- only valid until the next call unless retained
{# fun unsafe AssertString as assertString
    {`EnvironmentPtr', `CString'} -> `FactPtr' #}

-- | Determine an error from a call to assertString
{# fun unsafe GetAssertStringError as getAssertStringError
    {`EnvironmentPtr'} -> `AssertStringError' #}

-- | Create an instance from a string
{# fun unsafe MakeInstance as makeInstance
    {`EnvironmentPtr', `CString'} -> `InstancePtr' #}

-- | Determine an error from a call to makeInstance
{# fun unsafe GetMakeInstanceError as getMakeInstanceError
    {`EnvironmentPtr'} -> `MakeInstanceError' #}

-- | Retract a previously defined fact
{# fun unsafe Retract as retractFact
    {`FactPtr'} -> `RetractError' #}

-- | Remove a previously instantiated class member
{# fun unsafe UnmakeInstance as unmakeInstance
    {`InstancePtr'} -> `UnmakeInstanceError' #}

-- | Reset an environment, leaving deffacts, defrules, definstances, etc.
{# fun unsafe Reset as resetEnv
    {`EnvironmentPtr'} -> `()' #}

-- | Run an environment, at most n times. passing (-1) means infinitely
{# fun unsafe Run as runEnv
    {`EnvironmentPtr', fromIntegral `CLLong'} -> `CLLong' fromIntegral #}

-- | Enable dribbling to a file
{# fun unsafe DribbleOn as enableDribble
    {`EnvironmentPtr', `CString'} -> `Bool' #}

-- | Disable dribbline
{# fun unsafe DribbleOff as disableDribble
    {`EnvironmentPtr'} -> `Bool' #}

-- | Watch a specific type of expression
{# fun unsafe Watch as watchEnv
    {`EnvironmentPtr', `WatchItem'} -> `()' #}

-- | Remove a watch on a type of expression
{# fun unsafe Unwatch as unwatchEnv
    {`EnvironmentPtr', `WatchItem'} -> `()' #}
