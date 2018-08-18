{-# LANGUAGE TemplateHaskell #-}

-- | Concrete Error types
--
-- @since 0.1.0
module Failure.Error
  ( EitherE
  , Error
  , code
  , description
  , err
  , nextCause
  ) where

import Control.Lens (makeLenses, (%~), (^.))
import Failure.Fail (Fail (backtrace, cause, (+>)))
import Text.Printf  (printf)

-- | The Either type for Error
--
-- @since 0.1.0
type EitherE a b = Either (Error a) b

-- | The concrete error type
--
-- @since 0.1.0
data Error a = Error
  { _code        :: a -- ^ A generic error code
  , _description :: String -- ^ A generic string description
  , _nextCause   :: Maybe (Error a) -- ^ The next cause
  } deriving (Eq)

makeLenses ''Error

-- | The default string representation of an `Error`
--
-- @since 0.1.0
instance Show a => Show (Error a) where
  show Error {_code = e, _description = d, _nextCause = Just c} =
    printf "%s: %s" (printfE d e) $ show c
  show Error {_code = e, _description = d, _nextCause = Nothing} = printfE d e

-- | Convenience error printing helper
--
-- @since 0.1.0
printfE ::
     Show a
  => String -- ^ The description
  -> a -- ^ The error code
  -> String -- ^ The printed result
printfE s c = printf "%s (%s)" s (show c)

-- | Create a new `Error` for convenience
--
-- @since 0.1.0
new ::
     Maybe (Error a) -- ^ The possible cause for the Error
  -> a -- ^ The error code
  -> String -- ^ The description
  -> Error a -- ^ The resulting Error
new c d e = Error {_nextCause = c, _code = d, _description = e}

-- | This is a convenient way to turn a string into an error value that can be
-- passed around, if you do not want to create a new Fail type for this use
-- case.
--
-- @since 0.1.0
err ::
     a -- ^ The error code
  -> String -- ^ The description
  -> Error a -- ^ The resulting Error
err = new Nothing

-- | The `Fail` implementation for the concrete `Error` type
--
-- @since 0.1.0
instance Fail (Error a) where
  cause a = a ^. nextCause
  backtrace e@Error {_nextCause = Nothing} = [e]
  backtrace e@Error {_nextCause = Just c}  = e : backtrace c
  l +> r = nextCause %~ const (Just l) $ r
