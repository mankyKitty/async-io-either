---------------------------
-- |
-- These functions are partially inspired by the 'Catching All Exceptions' blog <https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions post>
-- that explains a useful way of pushing off a @IO a@ of a risky or smelly nature, and having it
-- provide a value indicating success or failure. This library does not go any further than taking
-- this result to a value, be it an exception or the desired result. You get an either, that's it.
--
-- It can be used where normally you might have to use @catch@ or similar mechanisms and you've already
-- built a @ExceptT@ having monad transformer stack in your application:
--
-- @
-- catch dangerous handleError
-- @
--
-- or if you've found yourself in the yucky position of having a @throw@ buried in your otherwise
-- pure code:
--
-- @
-- toMyError :: SomeException -> MyError
-- toMyError e = fromMaybe (throw e) <|> toMyError1 e <|> toMyError2 e
--   where
--     toMyError1 e' = Err1 <$> fromException e'
--     toMyError2 e' = Err2 <$> fromException e'
-- @
---------------------------
module Control.Concurrent.Async.Either
  ( runAsyncForE
  , runAsyncToE
  , handleRetryE
  , handleRetryToE
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class   (MonadIO, liftIO)

import           Control.Exception        (SomeException)
import           Data.Bifunctor           (first)

import           Control.Retry            (RetryPolicyM, RetryStatus,
                                           recoverAll)

-- |
-- Leverage the <http://hackage.haskell.org/package/retry retry> package to run your action
-- in a separate thread with a specific plan for retrying failures, yet capturing any exceptions
-- and returning the final result.
handleRetryE
  :: MonadIO m
  => RetryPolicyM IO
  -> (RetryStatus -> IO a)
  -> m (Either SomeException a)
handleRetryE rPolicy f =
  runAsyncForE ( recoverAll rPolicy f )

-- |
-- As per @handleRetryE@ but allows you to annotate or extract different error information from
-- the @SomeException@.
handleRetryToE
  :: MonadIO m
  => RetryPolicyM IO
  -> (RetryStatus -> IO a)
  -> (SomeException -> e)
  -> m (Either e a)
handleRetryToE rPolicy f eFn =
  runAsyncToE ( recoverAll rPolicy f ) eFn

-- |
-- The workhorse of this package, executes the @IO a@ in a separate thread
-- using @STM@. It should catch any and all exceptions in the process, even asynchronous ones!!
-- The result is a nice friendly @Either@ that you can then use however you desire. Instead of
-- things lurking around to break your nice Haskell code when you least expect it, BECAUSE IT WASN'T IN THE TYPE.
--
-- *mumble mumble*
runAsyncForE
  :: MonadIO m
  => IO a
  -> m (Either SomeException a)
runAsyncForE =
  liftIO . flip Async.withAsync Async.waitCatch

-- |
-- As per @runAsyncForE@ except you can attempt to turn the @SomeException@ into an Error type of your own.
-- Useful if there are instance of @Exception@ that you would like to annotate differently, or you have an
-- error type of your own that provides more context regarding what was going on at the time of the fire.
runAsyncToE
  :: MonadIO m
  => IO a
  -> (SomeException -> b)
  -> m (Either b a)
runAsyncToE rFn eFn =
  (first eFn) <$> runAsyncForE rFn
