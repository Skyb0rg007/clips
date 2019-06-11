-- | Core CLIPS functions
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE LambdaCase               #-}

module Clips.Core (
    -- * High-level
    -- ** Environments
      createEnv
    , destroyEnv
    , withEnv
    , loadEnvFromFile
    , eval_
    , eval
    , build
    -- ** Facts
    , assertString
    , assertString_
    -- ** Instances
    , makeInstance
    , makeInstance_
    -- ** Debugging
    , WatchItem (..)
    , watchEnv
    , unwatchEnv
    , enableDribble
    , disableDribble
    , peekClipsVal
    ) where

import           UnliftIO             (MonadUnliftIO)
import           UnliftIO.Exception   (Exception, bracket, throwIO)
import           Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import qualified Data.Text            as T
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

{# import        Clips.Error #}
{# import        Clips.Types #}

#include "clips.h"

-- | Turn *CLIPSValue into ClipsVal
peekClipsVal :: Environment -> Ptr ClipsVal -> IO ClipsVal
peekClipsVal env ptr = peek (castPtr ptr) >>= \case
    TH_VOID_TYPE             -> pure ClipsVoid
    TH_EXTERNAL_ADDRESS_TYPE -> ClipsPointer
        <$> peekByteOff ptr {# offsetof CLIPSExternalAddress->contents #}
    TH_SYMBOL_TYPE           -> do
        lexeme <- peekByteOff ptr {# offsetof CLIPSLexeme->contents #}
        ClipsSymbol . T.pack <$> peekCString lexeme
    TH_STRING_TYPE           -> do
        lexeme <- peekByteOff ptr {# offsetof CLIPSLexeme->contents #}
        ClipsString . T.pack <$> peekCString lexeme
    TH_INTEGER_TYPE          -> ClipsInteger . fromIntegral @CLLong
        <$> peekByteOff ptr {# offsetof CLIPSInteger->contents #}
    TH_FLOAT_TYPE            -> ClipsFloat . fromCDouble
        <$> peekByteOff ptr {# offsetof CLIPSFloat->contents #}
        where fromCDouble (CDouble f) = f
    TH_FACT_ADDRESS_TYPE     -> fmap ClipsFact . importFact env . castPtr $ ptr
    TH_INSTANCE_NAME_TYPE    -> fmap ClipsInstance . importInstance env . castPtr $ ptr
    TH_INSTANCE_ADDRESS_TYPE -> pure . ClipsPointer $ castPtr ptr
    TH_MULTIFIELD_TYPE       -> do
        -- len      <- peekByteOff ptr {# offsetof Multifield->length   #}
        contents <- peekByteOff ptr {# offsetof Multifield->contents #}
        pure . ClipsPointer $ castPtr (contents :: Ptr (Ptr ClipsVal))
        {- ClipsMultifield <$> ptrToList len contents
            where
                ptrToList :: CSize -> Ptr (Ptr ClipsVal) -> IO [ClipsVal]
                ptrToList n listPtr 
                  | n <= 0    = pure []
                  | otherwise = (:)
                        <$> peekClipsVal env (castPtr listPtr)
                        <*> ptrToList
                                (n - 1)
                                (listPtr `plusPtr` 8) -}

factFinalizer :: FinalizerEnvPtr Environment Fact
factFinalizer = releaseFactAddrC

importFact :: Environment -> Ptr Fact -> IO Fact
importFact (Environment env) ptr = do
    retainFactC (Environment env) ptr
    Fact <$> newForeignPtrEnv factFinalizer env ptr

instanceFinalizer :: FinalizerEnvPtr Environment Instance
instanceFinalizer = releaseInstanceAddrC

importInstance :: Environment -> Ptr Instance -> IO Instance
importInstance (Environment env) ptr = do
    retainInstanceC (Environment env) ptr
    Instance <$> newForeignPtrEnv instanceFinalizer env ptr

-- Helper to throw the given error as an IO exception of the given type
-- requires -XTypeApplications to use
throwCErr :: forall a b c m . (Enum a, Exception a, Integral b, MonadIO m)
          => b
          -> m c
throwCErr = (throwIO :: a -> m c) . toEnum . fromIntegral

-----------------------------------------------------------------------------

-- | Create a CLIPS environment
createEnv :: MonadIO m => m Environment
createEnv = liftIO createEnvironmentC

-- | Destroy a CLIPS environment
destroyEnv :: (MonadIO m) => Environment -> m Bool
destroyEnv env = liftIO $ toBool <$> destroyEnvironmentC env

-- | Run an action with an environment
withEnv :: MonadUnliftIO m => (Environment -> m a) -> m a
withEnv = bracket createEnv destroyEnv

-- | Load facts from a file into a CLIPS environment
loadEnvFromFile :: (MonadIO m, MonadReader Environment m)
                => FilePath
                -> m ()
loadEnvFromFile file = do
    env <- ask
    res <- liftIO $ withCString file $ loadC env
    if res == 0
       then pure ()
       else throwCErr @LoadError res

-- | Evaluate a command or function
eval_ :: (MonadIO m, MonadReader Environment m)
      => String
      -> m ()
eval_ str = do
    env <- ask
    res <- liftIO $ withCString str $ \str' -> evalC env str' nullPtr
    if res == 0
       then pure ()
       else throwCErr @EvalError res

eval :: (MonadIO m, MonadReader Environment m)
     => String
     -> m ClipsVal
eval str = do
    env <- ask
    liftIO $ alloca $ \ptr -> do
        res <- withCString str $ \str' -> evalC env str' ptr
        if res == 0
           then peekClipsVal env =<< peek ptr
           else throwCErr @EvalError res

-- | Build a rule or template
build :: (MonadIO m, MonadReader Environment m)
      => String
      -> m ()
build str = do
    env <- ask
    res <- liftIO $ withCString str $ buildC env
    if res == 0
       then pure ()
       else throwCErr @BuildError res

-- | Assert a fact
assertString_ :: (MonadIO m, MonadReader Environment m)
              => String
              -> m ()
assertString_ str = do
    env <- ask
    fact <- liftIO $ withCString str $ assertStringC env
    if fact /= nullPtr
       then pure ()
       else liftIO (getAssertStringErrorC env) >>= throwCErr @AssertStringError

-- | Assert a fact, returning a fact handle
assertString :: (MonadIO m, MonadReader Environment m)
             => String
             -> m Fact
assertString str = do
    env <- ask
    fact <- liftIO $ withCString str $ assertStringC env
    if fact /= nullPtr
       then liftIO $ importFact env fact
       else liftIO (getAssertStringErrorC env) >>= throwCErr @AssertStringError

makeInstance_ :: (MonadIO m, MonadReader Environment m)
              => String
              -> m ()
makeInstance_ str = do
    env <- ask
    inst <- liftIO $ withCString str $ makeInstanceC env
    if inst /= nullPtr
       then pure ()
       else liftIO (getMakeInstanceErrorC env) >>= throwCErr @MakeInstanceError

-- | Create an instance of a class, returning an instance handle
makeInstance :: (MonadIO m, MonadReader Environment m)
             => String
             -> m Instance
makeInstance str = do
    env <- ask
    inst <- liftIO $ withCString str $ makeInstanceC env
    if inst /= nullPtr
       then liftIO $ importInstance env inst
       else liftIO (getMakeInstanceErrorC env) >>= throwCErr @MakeInstanceError

{# enum WatchItem {} add prefix = "WI" #}

-- | Begin watching the environment for changes
watchEnv :: (MonadIO m, MonadReader Environment m)
         => WatchItem
         -> m ()
watchEnv wi = do
    env <- ask
    liftIO $ watchC env (fromIntegral $ fromEnum wi)

-- | Stop watching the environment for changes
unwatchEnv :: (MonadIO m, MonadReader Environment m)
           => WatchItem
           -> m ()
unwatchEnv wi = do
    env <- ask
    liftIO $ unwatchC env (fromIntegral $ fromEnum wi)

-- | Log to a file
enableDribble :: (MonadIO m, MonadReader Environment m)
              => FilePath
              -> m Bool
enableDribble str = do
    env <- ask
    liftIO $ fmap toBool $ withCString str $ dribbleOnC env

-- | Stop logging to a file
disableDribble :: (MonadIO m, MonadReader Environment m)
               => m Bool
disableDribble = liftIO . fmap toBool . dribbleOffC =<< ask

-- Environment
foreign import ccall unsafe "CreateEnvironment"
    createEnvironmentC :: IO Environment

foreign import ccall unsafe "DestroyEnvironment"
    destroyEnvironmentC :: Environment -> IO CBool

foreign import ccall unsafe "Load"
    loadC :: Environment -> CString -> IO {# type LoadError #}

foreign import ccall unsafe "Eval"
    evalC :: Environment -> CString -> Ptr (Ptr ClipsVal) -> IO {# type EvalError #}

foreign import ccall unsafe "Build"
    buildC :: Environment -> CString -> IO {# type BuildError #}

-- Fact
foreign import ccall unsafe "AssertString"
    assertStringC :: Environment -> CString -> IO (Ptr Fact)

foreign import ccall unsafe "GetAssertStringError"
    getAssertStringErrorC :: Environment -> IO {# type AssertStringError #}

foreign import ccall unsafe "RetainFact"
    retainFactC :: Environment -> Ptr Fact -> IO ()

foreign import ccall unsafe "&ReleaseFact"
    releaseFactAddrC :: FunPtr (Ptr Environment -> Ptr Fact -> IO ())

-- Instance
foreign import ccall unsafe "MakeInstance"
    makeInstanceC :: Environment -> CString -> IO (Ptr Instance)

foreign import ccall unsafe "GetMakeInstanceError"
    getMakeInstanceErrorC :: Environment -> IO {# type MakeInstanceError #}

foreign import ccall unsafe "RetainInstance"
    retainInstanceC :: Environment -> Ptr Instance -> IO ()

foreign import ccall unsafe "&ReleaseInstance"
    releaseInstanceAddrC :: FunPtr (Ptr Environment -> Ptr Instance -> IO ())

-- Debugging
foreign import ccall unsafe "Watch"
    watchC :: Environment -> {# type WatchItem #} -> IO ()

foreign import ccall unsafe "Unwatch"
    unwatchC :: Environment -> {# type WatchItem #} -> IO ()

foreign import ccall unsafe "DribbleOn"
    dribbleOnC :: Environment -> CString -> IO CBool

foreign import ccall unsafe "DribbleOff"
    dribbleOffC :: Environment -> IO CBool
