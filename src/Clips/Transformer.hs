-- | ClipsT monad transformer for easily running CLIPS functions
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clips.Transformer (
      ClipsT (..)
    , runClipsT
    , withClipsT
    ) where

import           Clips.Types                (Environment)
import           Clips.Core                 (withEnv)
import           Control.Applicative        (Alternative)
import           Control.Monad              (MonadPlus)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.Reader       (MonadReader, MonadTrans,
                                             ReaderT (..))
import           Control.Monad.Zip          (MonadZip)
import           Data.Functor.Contravariant (Contravariant)
import           UnliftIO                   (MonadIO, MonadUnliftIO (..),
                                             wrappedWithRunInIO)

newtype ClipsT m a = ClipsT { unClipsT :: ReaderT Environment m a }
    deriving ( Alternative
             , Applicative
             , Contravariant
             , Functor
             , Monad
             , MonadFail
             , MonadFix
             , MonadIO
             , MonadPlus
             , MonadReader Environment
             , MonadTrans
             , MonadZip
             )

instance MonadUnliftIO m => MonadUnliftIO (ClipsT m) where
    withRunInIO = wrappedWithRunInIO ClipsT unClipsT

runClipsT :: ClipsT m a -> Environment -> m a
runClipsT = runReaderT . unClipsT

withClipsT :: MonadUnliftIO m => ClipsT m a -> m a
withClipsT = withEnv . runClipsT
