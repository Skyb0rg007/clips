{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Clips.Raw.Advanced (
        -- * Eval and Build
          eval
        , build
        -- * FunctionCallBuilder
        , createFCB
        , disposeFCB
        , callFCB
        , resetFCB
        , fcbAppendVal
        , fcbAppendInteger
        , fcbAppendSymbol
        , fcbAppendString
        , fcbAppendFloat
        , fcbAppendInstanceName
        , fcbAppendClipsInteger
        , fcbAppendClipsFloat
        , fcbAppendClipsLexeme
        , fcbAppendMultifield
        , fcbAppendFact
        , fcbAppendInstance
        , fcbAppendClipsExternalAddress
        ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

{# import        Clips.Raw.Types #}

#include "clips.h"

-- fromCEnum :: Enum a => a -> CInt
-- fromCEnum = fromIntegral . fromEnum

toCEnum :: Enum a => CInt -> a
toCEnum = toEnum . fromIntegral

eval :: EnvironmentPtr -> String -> IO (Either EvalError ClipsValue)
eval env str =
    alloca $ \valPtr ->
        withCString str $ \cstr -> do
            err <- evalC env cstr valPtr
            if err /= 0
                then pure $ Left $ toCEnum err
                else Right <$> peek valPtr

foreign import ccall unsafe "Eval"
    evalC :: EnvironmentPtr -> CString -> Ptr ClipsValue -> IO {# type EvalError #}

build :: EnvironmentPtr -> String -> IO BuildError
build env str = fmap toCEnum . withCString str $
    {# call unsafe Build as buildC #} env


-- | Create a CLIPS function-builder
createFCB :: EnvironmentPtr -> Int -> IO FCBPtr
createFCB env n =
    {# call unsafe CreateFunctionCallBuilder as ^ #} env (fromIntegral n)

-- | Call a built function
callFCB :: FCBPtr -> String -> IO (Either FCBError ClipsValue)
callFCB fcb fnName =
    alloca $ \valPtr ->
        withCString fnName $ \fnCStr -> do
            err <- fcbCallC fcb fnCStr valPtr
            if err /= 0
                then pure $ Left $ toCEnum err
                else Right <$> peek valPtr

foreign import ccall unsafe "FCBCall"
    fcbCallC :: FCBPtr
             -> CString
             -> Ptr ClipsValue
             -> IO {# type FunctionCallBuilderError #}

-- | Reset a FCB
resetFCB :: FCBPtr -> IO ()
resetFCB = {# call unsafe FCBReset as ^ #}

-- | Free an FCB
disposeFCB :: FCBPtr -> IO ()
disposeFCB = {# call unsafe FCBDispose as ^ #}

-- | Append a ClipsValue to an FCB
fcbAppendVal :: FCBPtr -> ClipsValue -> IO ()
fcbAppendVal fcb val = with val $ fcbAppendC fcb

foreign import ccall unsafe "FCBAppend"
    fcbAppendC :: FCBPtr -> Ptr ClipsValue -> IO ()

-- | Append a UDFValue


-- | Append an integer
fcbAppendInteger :: FCBPtr -> Int -> IO ()
fcbAppendInteger fcb n =
    {# call unsafe FCBAppendInteger as ^ #} fcb (fromIntegral n)

-- | Append a float
fcbAppendFloat :: FCBPtr -> Double -> IO ()
fcbAppendFloat fcb n = {# call unsafe FCBAppendFloat as ^ #} fcb (CDouble n)

fcbAppendSymbol :: FCBPtr -> String -> IO ()
fcbAppendSymbol fcb str = withCString str $
    {# call unsafe FCBAppendSymbol as ^ #} fcb

fcbAppendString :: FCBPtr -> String -> IO ()
fcbAppendString fcb str = withCString str $
    {# call unsafe FCBAppendString as ^ #} fcb

fcbAppendInstanceName :: FCBPtr -> String -> IO ()
fcbAppendInstanceName fcb str = withCString str $
    {# call unsafe FCBAppendInstanceName as ^ #} fcb

fcbAppendClipsInteger :: FCBPtr -> ClipsInteger -> IO ()
fcbAppendClipsInteger fcb ci = with ci $ fcbAppendClipsIntegerC fcb

foreign import ccall unsafe "FCBAppendCLIPSInteger"
    fcbAppendClipsIntegerC :: FCBPtr -> Ptr ClipsInteger -> IO ()

fcbAppendClipsFloat :: FCBPtr -> ClipsFloat -> IO ()
fcbAppendClipsFloat fcb cf = with cf $ fcbAppendClipsFloatC fcb

foreign import ccall unsafe "FCBAppendCLIPSFloat"
    fcbAppendClipsFloatC :: FCBPtr -> Ptr ClipsFloat -> IO ()

fcbAppendClipsLexeme :: FCBPtr -> ClipsLexeme -> IO ()
fcbAppendClipsLexeme fcb cl = with cl $ fcbAppendClipsLexemeC fcb

foreign import ccall unsafe "FCBAppendCLIPSLexeme"
    fcbAppendClipsLexemeC :: FCBPtr -> Ptr ClipsLexeme -> IO ()

fcbAppendFact :: FCBPtr -> FactPtr -> IO ()
fcbAppendFact = {# call unsafe FCBAppendFact as ^ #}

fcbAppendInstance :: FCBPtr -> InstancePtr -> IO ()
fcbAppendInstance = {# call unsafe FCBAppendInstance as ^ #}

fcbAppendMultifield :: FCBPtr -> ClipsMultifield -> IO ()
fcbAppendMultifield fcb mf = with mf $ fcbAppendMultifieldC fcb

foreign import ccall unsafe "FCBAppendMultifield"
    fcbAppendMultifieldC :: FCBPtr -> Ptr ClipsMultifield -> IO ()

fcbAppendClipsExternalAddress :: FCBPtr -> ClipsExternalAddress -> IO ()
fcbAppendClipsExternalAddress fcb ea = with ea $
    fcbAppendClipsExternalAddressC fcb

foreign import ccall unsafe "FCBAppendCLIPSExternalAddress"
    fcbAppendClipsExternalAddressC :: FCBPtr
                                   -> Ptr ClipsExternalAddress
                                   -> IO ()
