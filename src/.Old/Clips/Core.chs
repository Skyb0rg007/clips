{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Clips.Core where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Clips.Primatives

#include "clips.h"

{#enum ASE_NO_ERROR as AssertStringError {underscoreToCase}
    deriving (Show, Eq) #}

{#enum MIE_NO_ERROR as MakeInstanceError {underscoreToCase}
    deriving (Show, Eq) #}

{#enum RE_NO_ERROR as RetractError {underscoreToCase}
    deriving (Show, Eq) #}

{#enum UIE_NO_ERROR as UnmakeInstanceError {underscoreToCase}
    deriving (Show, Eq) #}

{#enum GENERIC_FUNCTIONS as WatchItem {underscoreToCase}
    deriving (Show, Eq) #}

-- * Core functions

-- ** Creating and Destroying Environments

foreign import ccall unsafe "CreateEnvironment"
    createEnvironment :: IO (Ptr Environment)

foreign import ccall unsafe "DestroyEnvironment"
    destroyEnvironment :: Ptr Environment -> IO ()

-- ** Loading Constructs

foreign import ccall unsafe "Clear"
    clearEnvironment :: Ptr Environment -> IO ()

foreign import ccall unsafe "Load"
    loadEnvironment :: Ptr Environment -> CString -> IO ()

-- ** Creating and Removing Facts and Instances

foreign import ccall unsafe "AssertString"
    assertString :: Ptr Environment -> CString -> IO (Ptr Fact)

foreign import ccall unsafe "GetAssertStringError"
    getAssertStringError :: Ptr Environment -> IO {#type AssertStringError#}

foreign import ccall unsafe "MakeInstance"
    makeInstance :: Ptr Environment -> CString -> IO (Ptr Instance)

foreign import ccall unsafe "GetMakeInstanceError"
    getMakeInstanceError :: Ptr Environment -> IO {#type MakeInstanceError#}

foreign import ccall unsafe "Retract"
    retract :: Ptr Fact -> IO {#type RetractError#}

foreign import ccall unsafe "UnmakeInstance"
    unmakeInstance :: Ptr Instance -> IO {#type UnmakeInstanceError#}

-- ** Executing Rules

foreign import ccall unsafe "Reset"
    reset :: Ptr Environment -> IO ()

foreign import ccall unsafe "Run"
    run :: Ptr Environment -> CLLong -> IO CLLong

-- ** Debugging

foreign import ccall unsafe "DribbleOn"
    dribbleOn :: Ptr Environment -> CString -> IO Bool

foreign import ccall unsafe "DribbleOff"
    dribbleOff :: Ptr Environment -> IO Bool

foreign import ccall unsafe "Watch"
    watch :: Ptr Environment -> {#type WatchItem#} -> IO ()

foreign import ccall unsafe "Unwatch"
    unwatch :: Ptr Environment -> {#type WatchItem#} -> IO ()


-- * Eval and Build

{#enum EvalError {underscoreToCase}
    deriving (Show, Eq) #}

{#enum BuildError {underscoreToCase}
    deriving (Show, Eq) #}

foreign import ccall unsafe "Eval"
    eval :: Ptr Environment -> CString -> Ptr ClipsValue -> IO {#type EvalError#}

foreign import ccall unsafe "Build"
    build :: Ptr Environment -> CString -> IO {#type BuildError#}


