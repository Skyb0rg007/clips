-- | CLIPS error values and helpers
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Clips.Error (
    -- * Error Types
      LoadError (..)
    , AssertStringError (..)
    , MakeInstanceError (..)
    , RetractError (..)
    , UnmakeInstanceError (..)
    , EvalError (..)
    , BuildError (..)
    , FunctionCallBuilderError (..)
    -- * Typeclass for all clips errors
    , ClipsError
    , catchClips
    , tryClips
    ) where

import           Data.Typeable      (Typeable)
import           UnliftIO           (MonadUnliftIO, Exception, catch, try)

#include "clips.h"

-- Error types

class (Eq e, Exception e) => ClipsError e

catchClips :: (MonadUnliftIO m, ClipsError e)
           => m a
           -> (e -> m a)
           -> m a
catchClips = catch

tryClips :: (MonadUnliftIO m, ClipsError e)
         => m a
         -> m (Either e a)
tryClips = try

{# enum LoadError                {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

{# enum AssertStringError        {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

{# enum MakeInstanceError        {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

{# enum RetractError             {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

{# enum UnmakeInstanceError      {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

{# enum EvalError                {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

{# enum BuildError               {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

{# enum FunctionCallBuilderError {underscoreToCase}
    omit (LE_NO_ERROR)
    deriving (Show, Eq, Typeable, Exception, ClipsError) #}

