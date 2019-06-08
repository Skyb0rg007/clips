{-# LANGUAGE CPP                        #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clips.Primatives where

import           Data.Void        (Void)
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include "clips.h"


data Environment
data Fact
data Instance


{#enum define ValueType
    { EXTERNAL_ADDRESS_TYPE as ExternalAddressType
    , FACT_ADDRESS_TYPE     as FactAddressType
    , FLOAT_TYPE            as FloatType
    , INSTANCE_ADDRESS_TYPE as InstanceAddressType
    , INSTANCE_NAME_TYPE    as InstanceNameType
    , INTEGER_TYPE          as IntegerType
    , MULTIFIELD_TYPE       as MultifieldType
    , STRING_TYPE           as StringType
    , SYMBOL_TYPE           as SymbolType
    , VOID_TYPE             as VoidType
    } deriving (Show, Eq) #}

newtype TypeHeader = TypeHeader CUShort
    deriving (Show, Eq, Storable)

-- typedef struct clipsValue {
--   union {
--     void *value;
--     TypeHeader *header;
--     CLIPSLexeme *lexemeValue;
--     CLIPSFloat *floatValue;
--     CLIPSInteger *integerValue;
--     CLIPSVoid *voidValue;
--     Fact *factValue;
--     Instance *instanceValue;
--     Multifield *multifieldValue;
--     CLIPSExternalAddress *externalAddressValue;
--   };
-- } CLIPSValue;

newtype ClipsValue = ClipsValue (Ptr Void)
    deriving (Show, Eq, Storable)

-- CLIPSLexeme *lexemeValue;
data ClipsLexeme = ClipsLexeme TypeHeader CString
    deriving (Show, Eq)

instance Storable ClipsLexeme where
    sizeOf _ = {#sizeof CLIPSLexeme#}
    alignment _ = {#alignof CLIPSLexeme#}
    peek ptr = ClipsLexeme
        <$> peekByteOff ptr {#offsetof CLIPSLexeme->header#}
        <*> peekByteOff ptr {#offsetof CLIPSLexeme->contents#}
    poke ptr (ClipsLexeme header contents) = do
        pokeByteOff ptr {#offsetof CLIPSLexeme->header#}   header
        pokeByteOff ptr {#offsetof CLIPSLexeme->contents#} contents

-- CLIPSFloat *floatValue;
data ClipsFloat = ClipsFloat TypeHeader CDouble
    deriving (Show, Eq)

instance Storable ClipsFloat where
    sizeOf _ = {#sizeof CLIPSFloat#}
    alignment _ = {#alignof CLIPSFloat#}
    peek ptr = ClipsFloat
        <$> peekByteOff ptr {#offsetof CLIPSFloat->header#}
        <*> peekByteOff ptr {#offsetof CLIPSFloat->contents#}
    poke ptr (ClipsFloat header contents) = do
        pokeByteOff ptr {#offsetof CLIPSFloat->header#}   header
        pokeByteOff ptr {#offsetof CLIPSFloat->contents#} contents

-- CLIPSInteger *integerValue;
data ClipsInteger = ClipsInteger TypeHeader CLLong
    deriving (Show, Eq)

instance Storable ClipsInteger where
    sizeOf _ = {#sizeof CLIPSInteger#}
    alignment _ = {#alignof CLIPSInteger#}
    peek ptr = ClipsInteger
        <$> peekByteOff ptr {#offsetof CLIPSInteger->header#}
        <*> peekByteOff ptr {#offsetof CLIPSInteger->contents#}
    poke ptr (ClipsInteger header contents) = do
        pokeByteOff ptr {#offsetof CLIPSInteger->header#}   header
        pokeByteOff ptr {#offsetof CLIPSInteger->contents#} contents

-- CLIPSVoid *voidValue;
newtype ClipsVoid = ClipsVoid TypeHeader
    deriving (Show, Eq, Storable)

-- CLIPSMultifield *multifieldValue;
data ClipsMultifield = ClipsMultifield TypeHeader CSize (Ptr ClipsValue)
    deriving (Show, Eq)

instance Storable ClipsMultifield where
    sizeOf _ = {#sizeof Multifield#}
    alignment _ = {#alignof Multifield#}
    peek ptr = ClipsMultifield
        <$> peekByteOff ptr {#offsetof Multifield->header#}
        <*> peekByteOff ptr {#offsetof Multifield->length#}
        <*> peekByteOff ptr {#offsetof Multifield->contents#}
    poke ptr (ClipsMultifield header len contents) = do
        pokeByteOff ptr {#offsetof Multifield->header#}   header
        pokeByteOff ptr {#offsetof Multifield->length#}   len
        pokeByteOff ptr {#offsetof Multifield->contents#} contents

-- CLIPSExternalAddress *externalAddressValue;
data ClipsExternalAddress = ClipsExternalAddress TypeHeader (Ptr Void)
    deriving (Show, Eq)

instance Storable ClipsExternalAddress where
    sizeOf _ = {#sizeof CLIPSExternalAddress#}
    alignment _ = {#alignof CLIPSExternalAddress#}
    peek ptr = ClipsExternalAddress
        <$> peekByteOff ptr {#offsetof CLIPSExternalAddress->header#}
        <*> peekByteOff ptr {#offsetof CLIPSExternalAddress->contents#}
    poke ptr (ClipsExternalAddress header contents) = do
        pokeByteOff ptr {#offsetof CLIPSExternalAddress->header#}   header
        pokeByteOff ptr {#offsetof CLIPSExternalAddress->contents#} contents
















