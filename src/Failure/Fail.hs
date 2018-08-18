-- | The interface for Fail types
--
-- @since 0.1.0
module Failure.Fail
  ( Fail(backtrace, cause, (+>))
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
-- `+>` adds an Error to the chain. This sets the seconds error cause to the
-- first one and prepends it therefore to the overall error-chain.
--
-- @since 0.1.0
class Fail a where
  backtrace ::
       a -- ^ The input failure
    -> [a] -- ^ The resulting list of failures as backtrace
  backtrace _ = []
  cause ::
       a -- ^ The input failure
    -> Maybe a -- ^ The cause failure
  cause _ = Nothing
  (+>) ::
       a -- ^ The reason for the resulting failure
    -> a -- ^ The failure to append
    -> a -- ^ The resulting failure
  _ +> r = r
