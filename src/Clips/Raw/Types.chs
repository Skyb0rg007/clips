{-# LANGUAGE CPP                        #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}

module Clips.Raw.Types (
        -- * Opaque pointer types
          Environment
        , EnvironmentPtr
        , Fact
        , FactPtr
        , Instance
        , InstancePtr
        -- * Error types
        , LoadError (..)
        , AssertStringError (..)
        , MakeInstanceError (..)
        , RetractError (..)
        , UnmakeInstanceError (..)
        -- * CLIPS Types
        , TypeHeader ( TH_EXTERNAL_ADDRESS_TYPE
                     , TH_FACT_ADDRESS_TYPE
                     , TH_FLOAT_TYPE
                     , TH_INSTANCE_ADDRESS_TYPE
                     , TH_INSTANCE_NAME_TYPE
                     , TH_INTEGER_TYPE
                     , TH_MULTIFIELD_TYPE
                     , TH_STRING_TYPE
                     , TH_SYMBOL_TYPE
                     , TH_VOID_TYPE
                     )
        -- * Miscellaneous
        , WatchItem (..)
        ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Data.Void (Void)

#include "clips.h"

data Environment
data Fact
data Instance
{# pointer *Environment as EnvironmentPtr -> Environment #}
{# pointer *Fact        as FactPtr        -> Fact        #}
{# pointer *Instance    as InstancePtr    -> Instance    #}

{# enum LoadError           {} #}
{# enum AssertStringError   {} #}
{# enum MakeInstanceError   {} #}
{# enum RetractError        {} #}
{# enum UnmakeInstanceError {} #}
{# enum WatchItem           {} add prefix = "WI_" #}

newtype TypeHeader = TypeHeader CUShort
    deriving (Storable)
pattern TH_EXTERNAL_ADDRESS_TYPE :: TypeHeader
pattern TH_EXTERNAL_ADDRESS_TYPE = TypeHeader {#const EXTERNAL_ADDRESS_TYPE#}
pattern TH_FACT_ADDRESS_TYPE     :: TypeHeader
pattern TH_FACT_ADDRESS_TYPE     = TypeHeader {#const FACT_ADDRESS_TYPE    #}
pattern TH_FLOAT_TYPE            :: TypeHeader
pattern TH_FLOAT_TYPE            = TypeHeader {#const FLOAT_TYPE           #}
pattern TH_INSTANCE_ADDRESS_TYPE :: TypeHeader
pattern TH_INSTANCE_ADDRESS_TYPE = TypeHeader {#const INSTANCE_ADDRESS_TYPE#}
pattern TH_INSTANCE_NAME_TYPE    :: TypeHeader
pattern TH_INSTANCE_NAME_TYPE    = TypeHeader {#const INSTANCE_NAME_TYPE   #}
pattern TH_INTEGER_TYPE          :: TypeHeader
pattern TH_INTEGER_TYPE          = TypeHeader {#const INTEGER_TYPE         #}
pattern TH_MULTIFIELD_TYPE       :: TypeHeader
pattern TH_MULTIFIELD_TYPE       = TypeHeader {#const MULTIFIELD_TYPE      #}
pattern TH_STRING_TYPE           :: TypeHeader
pattern TH_STRING_TYPE           = TypeHeader {#const STRING_TYPE          #}
pattern TH_SYMBOL_TYPE           :: TypeHeader
pattern TH_SYMBOL_TYPE           = TypeHeader {#const SYMBOL_TYPE          #}
pattern TH_VOID_TYPE             :: TypeHeader
pattern TH_VOID_TYPE             = TypeHeader {#const VOID_TYPE            #}
{-# COMPLETE TH_EXTERNAL_ADDRESS_TYPE
           , TH_FACT_ADDRESS_TYPE
           , TH_FLOAT_TYPE
           , TH_INSTANCE_ADDRESS_TYPE
           , TH_INSTANCE_NAME_TYPE
           , TH_INTEGER_TYPE
           , TH_MULTIFIELD_TYPE
           , TH_STRING_TYPE
           , TH_SYMBOL_TYPE
           , TH_VOID_TYPE #-}

data ClipsValue
    = ClipsLexeme !CString
    | ClipsFloat !CDouble
    | ClipsInteger !CLLong
    | ClipsVoid
    | ClipsFact !(Ptr Fact)
    | ClipsInstance !(Ptr Instance)
    | ClipsMultifield !CSize !(Ptr ClipsValue)
    | ClipsExternalAddress !(Ptr Void)

instance Storable ClipsValue where
    sizeOf _ = {#sizeof CLIPSValue#}
    alignment _ = {#alignof CLIPSValue#}
    peek ptr = undefined
    poke ptr _ = undefined

valueFromPtr :: Ptr TypeHeader -> IO ClipsValue
valueFromPtr ptr = peek ptr >>= \case
    TH_VOID_TYPE -> pure ClipsVoid
    TH_EXTERNAL_ADDRESS_TYPE -> ClipsExternalAddress
        <$> peekByteOff ptr {#offsetof CLIPSExternalAddress->contents#}
    TH_FACT_ADDRESS_TYPE -> pure $ ClipsFact $ castPtr ptr
    TH_INSTANCE_ADDRESS_TYPE -> pure $ ClipsInstance $ castPtr ptr
    TH_FLOAT_TYPE -> ClipsFloat
        <$> peekByteOff ptr {#offsetof CLIPSFloat->contents#}
    _ -> pure ClipsVoid
    -- TODO

