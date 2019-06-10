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
        , FCB
        , FCBPtr
        -- * Error types
        , LoadError (..)
        , AssertStringError (..)
        , MakeInstanceError (..)
        , RetractError (..)
        , UnmakeInstanceError (..)
        , EvalError (..)
        , BuildError (..)
        , FCBError (..)
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
        , ClipsLexeme (..)
        , ClipsInteger (..)
        , ClipsFloat (..)
        , ClipsMultifield (..)
        , ClipsValue (..)
        , ClipsExternalAddress (..)
        -- * watchEnv options
        , WatchItem (..)
        ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include "clips.h"

-- | Opaque type representing all data about the running CLIPS system
data Environment

-- | Opaque type representing a fact in the CLIPS system
data Fact

-- | Opaque type representing an instance in the CLIPS system
data Instance

-- | Opaque type representing a function call builder
data FCB

{# pointer *Environment         as EnvironmentPtr -> Environment #}
{# pointer *Fact                as FactPtr        -> Fact        #}
{# pointer *Instance            as InstancePtr    -> Instance    #}
{# pointer *FunctionCallBuilder as FCBPtr         -> FCB         #}

-- | Return type for loadEnvFromFile
{# enum LoadError           {} deriving (Show, Eq) #}
-- | Return type for assertString
{# enum AssertStringError   {} omit (ASE_NO_ERROR) deriving (Show, Eq) #}
-- | Return type for makeInstance
{# enum MakeInstanceError   {} omit (MIE_NO_ERROR) deriving (Show, Eq) #}
-- | Return type for retractFact
{# enum RetractError        {} deriving (Show, Eq) #}
-- | Return type for unmakeInstance
{# enum UnmakeInstanceError {} deriving (Show, Eq) #}
-- | Input type for watchEnv
{# enum WatchItem           {} add prefix = "WI_" deriving (Show, Eq) #}
-- | Return type for eval
{# enum EvalError           {} omit (EE_NO_ERROR) deriving (Show, Eq) #}
-- | Return type for build
{# enum BuildError          {} deriving (Show, Eq) #}
-- | Return type for callFCB
{# enum FunctionCallBuilderError as FCBError {} deriving (Show, Eq) #}

-- | The header that determines the type of a ClipsValue
{# enum define TypeHeader
    { EXTERNAL_ADDRESS_TYPE as TH_EXTERNAL_ADDRESS_TYPE
    , VOID_TYPE             as TH_VOID_TYPE
    , SYMBOL_TYPE           as TH_SYMBOL_TYPE
    , STRING_TYPE           as TH_STRING_TYPE
    , MULTIFIELD_TYPE       as TH_MULTIFIELD_TYPE
    , INTEGER_TYPE          as TH_INTEGER_TYPE
    , INSTANCE_NAME_TYPE    as TH_INSTANCE_NAME_TYPE
    , INSTANCE_ADDRESS_TYPE as TH_INSTANCE_ADDRESS_TYPE
    , FLOAT_TYPE            as TH_FLOAT_TYPE
    , FACT_ADDRESS_TYPE     as TH_FACT_ADDRESS_TYPE
    } deriving (Show, Eq)
    #}

instance Storable TypeHeader where
    sizeOf _ = {# sizeof TypeHeader #}
    alignment _ = {# alignof TypeHeader #}
    peek = fmap fromUShort . peek . castPtr
      where
        fromUShort :: CUShort -> TypeHeader
        fromUShort = toEnum . fromIntegral
    poke ptr = poke (castPtr ptr) . toUShort
      where
        toUShort :: TypeHeader -> CUShort
        toUShort = toEnum . fromEnum

-- | Representation of a ClipsLexeme
data ClipsLexeme = ClipsSymbol !CString | ClipsString !CString
    deriving (Show, Eq)

instance Storable ClipsLexeme where
    sizeOf _ = {# sizeof CLIPSLexeme #}
    alignment _ = {# alignof CLIPSLexeme #}
    peek ptr = do
        hdr  <- peek $ castPtr ptr
        cstr <- peekByteOff ptr {# offsetof CLIPSLexeme->contents #}
        case hdr of
            TH_SYMBOL_TYPE -> pure $ ClipsSymbol cstr
            TH_STRING_TYPE -> pure $ ClipsString cstr
            _ -> error "Storable.peek: ClipsLexeme is in invalid state"
    poke ptr (ClipsString cstr) = do
        pokeByteOff ptr {# offsetof CLIPSLexeme->header   #} TH_STRING_TYPE
        pokeByteOff ptr {# offsetof CLIPSLexeme->contents #} cstr
    poke ptr (ClipsSymbol cstr) = do
        pokeByteOff ptr {# offsetof CLIPSLexeme->header   #} TH_SYMBOL_TYPE
        pokeByteOff ptr {# offsetof CLIPSLexeme->contents #} cstr

-- | Representation of a ClipsInteger
data ClipsInteger = ClipsInteger !CLLong
    deriving (Show, Eq)

instance Storable ClipsInteger where
    sizeOf _ = {# sizeof CLIPSInteger #}
    alignment _ = {# alignof CLIPSInteger #}
    peek ptr = ClipsInteger
        <$> peekByteOff ptr {# offsetof CLIPSInteger->contents #}
    poke ptr (ClipsInteger val) = do
        pokeByteOff ptr {# offsetof CLIPSInteger->header   #} TH_INTEGER_TYPE
        pokeByteOff ptr {# offsetof CLIPSInteger->contents #} val

-- | Representation of a ClipsFloat
data ClipsFloat = ClipsFloat !CDouble
    deriving (Show, Eq)

instance Storable ClipsFloat where
    sizeOf _ = {# sizeof CLIPSFloat #}
    alignment _ = {# alignof CLIPSFloat #}
    peek ptr = ClipsFloat 
        <$> peekByteOff ptr {# offsetof CLIPSFloat->contents #}
    poke ptr (ClipsFloat val) = do
        pokeByteOff ptr {# offsetof CLIPSFloat->header   #} TH_FLOAT_TYPE
        pokeByteOff ptr {# offsetof CLIPSFloat->contents #} val

-- | Representation of a ClipsMultifield
data ClipsMultifield = ClipsMultifield !CSize !(Ptr ())
    deriving (Show, Eq)

instance Storable ClipsMultifield where
    sizeOf _ = {# sizeof Multifield #}
    alignment _ = {# alignof Multifield #}
    peek mPtr = ClipsMultifield
        <$> peekByteOff mPtr {# offsetof Multifield->length #}
        <*> {# get Multifield->contents #} mPtr
    poke ptr (ClipsMultifield len contents) = do
        pokeByteOff ptr {# offsetof Multifield->header   #} TH_MULTIFIELD_TYPE
        pokeByteOff ptr {# offsetof Multifield->length   #} len
        pokeByteOff ptr {# offsetof Multifield->contents #} contents

-- | Representation of a ClipsVoid
data ClipsVoid = ClipsVoid
    deriving (Show, Eq)

instance Storable ClipsVoid where
    sizeOf _ = {# sizeof CLIPSVoid #}
    alignment _ = {# alignof CLIPSVoid #}
    peek _ = pure ClipsVoid
    poke ptr _ = poke (castPtr ptr) TH_VOID_TYPE

-- | Representation of a ClipsExternalAddress
data ClipsExternalAddress = ClipsExternalAddress !(Ptr ())
    deriving (Show, Eq)

instance Storable ClipsExternalAddress where
    sizeOf _ = {# sizeof CLIPSExternalAddress #}
    alignment _ = {# alignof CLIPSExternalAddress #}
    peek ptr = ClipsExternalAddress
        <$> peekByteOff ptr {# offsetof CLIPSExternalAddress->contents #}
    poke ptr (ClipsExternalAddress addr) = do
        pokeByteOff ptr {# offsetof CLIPSExternalAddress->header   #} TH_EXTERNAL_ADDRESS_TYPE
        pokeByteOff ptr {# offsetof CLIPSExternalAddress->contents #} addr

-- | Representation of a ClipsValue
data ClipsValue
    = CVPtr             !(Ptr ())
    | CVHeader          !(Ptr TypeHeader)
    | CVLexeme          !(Ptr ClipsLexeme)
    | CVFloat           !(Ptr ClipsFloat)
    | CVInteger         !(Ptr ClipsInteger)
    | CVVoid            !(Ptr ClipsVoid)
    | CVFact            !FactPtr
    | CVInstance        !InstancePtr
    | CVMultifield      !(Ptr ClipsMultifield)
    | CVExternalAddress !(Ptr ClipsExternalAddress)
    deriving (Show, Eq)

instance Storable ClipsValue where
    sizeOf _ = {# sizeof CLIPSValue #}
    alignment _ = {# alignof CLIPSValue #}
    peek ptr = peek (castPtr ptr :: Ptr (Ptr TypeHeader)) >>= peek >>= \case
        TH_FLOAT_TYPE            -> CVFloat           <$> peek (castPtr ptr)
        TH_EXTERNAL_ADDRESS_TYPE -> CVExternalAddress <$> peek (castPtr ptr)
        TH_VOID_TYPE             -> CVVoid            <$> peek (castPtr ptr)
        TH_MULTIFIELD_TYPE       -> CVMultifield      <$> peek (castPtr ptr)
        TH_SYMBOL_TYPE           -> CVLexeme          <$> peek (castPtr ptr)
        TH_STRING_TYPE           -> CVLexeme          <$> peek (castPtr ptr)
        TH_INTEGER_TYPE          -> CVInteger         <$> peek (castPtr ptr)
        TH_INSTANCE_NAME_TYPE    -> CVInstance        <$> peek (castPtr ptr)
        TH_INSTANCE_ADDRESS_TYPE -> CVInstance        <$> peek (castPtr ptr)
        TH_FACT_ADDRESS_TYPE     -> CVFact            <$> peek (castPtr ptr)
    poke ptr (CVPtr val)             = poke (castPtr ptr) val
    poke ptr (CVHeader val)          = poke (castPtr ptr) val
    poke ptr (CVLexeme val)          = poke (castPtr ptr) val
    poke ptr (CVFloat val)           = poke (castPtr ptr) val
    poke ptr (CVInteger val)         = poke (castPtr ptr) val
    poke ptr (CVVoid val)            = poke (castPtr ptr) val
    poke ptr (CVFact val)            = poke (castPtr ptr) val
    poke ptr (CVInstance val)        = poke (castPtr ptr) val
    poke ptr (CVMultifield val)      = poke (castPtr ptr) val
    poke ptr (CVExternalAddress val) = poke (castPtr ptr) val

{-
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
        sizeOf _ = {# sizeof CLIPSValue #}
    alignment _ = {# alignof CLIPSValue #}
    peek = undefined
        poke _ = undefined

        valueFromPtr :: Ptr TypeHeader -> IO ClipsValue
        valueFromPtr ptr = peek ptr >>= \case
        TH_VOID_TYPE -> pure ClipsVoid
        TH_EXTERNAL_ADDRESS_TYPE -> ClipsExternalAddress
        <$> peekByteOff ptr {# offsetof CLIPSExternalAddress->contents #}
    TH_FACT_ADDRESS_TYPE -> pure $ ClipsFact $ castPtr ptr
        TH_INSTANCE_ADDRESS_TYPE -> pure $ ClipsInstance $ castPtr ptr
        TH_FLOAT_TYPE -> ClipsFloat
        <$> peekByteOff ptr {# offsetof CLIPSFloat->contents #}
    _ -> pure ClipsVoid
        -- TODO
-}
