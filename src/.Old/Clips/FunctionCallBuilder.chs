{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Clips.FunctionCallBuilder where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Clips.Primatives

data FunctionCallBuilder


foreign import ccall unsafe "CreateFunctionCallBuilder"
    createFunctionCallBuilder :: Ptr Environment
                              -> CSize
                              -> IO (Ptr FunctionCallBuilder)

foreign import ccall unsafe "FCBCall"
    fcbCall :: Ptr FunctionCallBuilder
            -> CString
            -> IO (Ptr ClipsValue)

foreign import ccall unsafe "FCBReset"
    fcbReset :: Ptr FunctionCallBuilder -> IO ()

foreign import ccall unsafe "FCBDispose"
    fcbDispose :: Ptr FunctionCallBuilder -> IO ()

foreign import ccall unsafe "FCBAppend"
    fcbAppend :: Ptr FunctionCallBuilder -> Ptr ClipsValue -> IO ()


