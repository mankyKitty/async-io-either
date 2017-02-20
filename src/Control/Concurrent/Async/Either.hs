{-# LANGUAGE CPP #-}
module Control.Concurrent.Async.Either
  ( runAsyncForE
  , runAsyncToE
#if VERSION_retry
  , handleRetryE
  , handleRetryToE
#endif
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class   (MonadIO, liftIO)

import           Control.Exception        (SomeException)
import           Data.Bifunctor           (first)

#if VERSION_retry
import           Control.Retry            (RetryPolicyM, RetryStatus,
                                           recoverAll)
#endif

#if VERSION_retry
handleRetryE
  :: MonadIO m
  => RetryPolicyM IO
  -> (RetryStatus -> IO a)
  -> m (Either SomeException a)
handleRetryE rPolicy f =
  runAsyncForE ( recoverAll rPolicy f )

handleRetryToE
  :: MonadIO m
  => RetryPolicyM IO
  -> (RetryStatus -> IO a)
  -> (SomeException -> e)
  -> m (Either e a)
handleRetryToE rPolicy f eFn =
  runAsyncToE ( recoverAll rPolicy f ) eFn
#endif

runAsyncForE
  :: MonadIO m
  => IO a
  -> m (Either SomeException a)
runAsyncForE =
  liftIO . flip Async.withAsync Async.waitCatch

runAsyncToE
  :: MonadIO m
  => IO a
  -> (SomeException -> b)
  -> m (Either b a)
runAsyncToE rFn eFn =
  (first eFn) <$> runAsyncForE rFn
