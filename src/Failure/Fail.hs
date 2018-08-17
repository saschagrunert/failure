-- | The interface for Fail types
--
-- @since 0.1.0
module Failure.Fail
  ( Fail(backtrace, cause)
  ) where

-- | The main class of Fails.
--
-- Types which implement `Fail` are called failures.
--
-- `backtrace` evaluates to the ordered list of failures
--
-- `cause` returns the underlying cause of this failure, if it is an error that
-- wraps other errors. Returns `Nothing` if this failure does not have
-- another error as its underlying cause.
--
-- @since 0.1.0
class Fail a where
  backtrace :: a -> [a]
  backtrace _ = []
  cause :: a -> Maybe a
  cause _ = Nothing
