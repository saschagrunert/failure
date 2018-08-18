{-# LANGUAGE TemplateHaskell #-}

-- | Concrete Error types
--
-- @since 0.1.0
module Failure.Error
  ( Code
  , Error
  , code
  , description
  , err
  , errAppend
  , nextCause
  ) where

import Control.Lens (makeLenses, (^.))
import Failure.Fail (Fail (backtrace, cause))
import Text.Printf  (printf)

-- | Simple error code to integer representation
--
-- @since 0.1.0
type Code = Integer

-- | The concrete error type
--
-- @since 0.1.0
data Error = Error
  { _code        :: Code -- ^ A generic error code
  , _description :: String -- ^ A generic string description
  , _nextCause   :: Maybe Error -- ^ The next cause
  } deriving (Eq)

makeLenses ''Error

-- | The default string representation of an `Error`
--
-- @since 0.1.0
instance Show Error where
  show Error {_code = e, _description = d, _nextCause = Just c} =
    printf "%s: %s" (printfE d e) $ show c
  show Error {_code = e, _description = d, _nextCause = Nothing} = printfE d e

-- | Convenience error printing helper
--
-- @since 0.1.0
printfE :: String -> Code -> String
printfE = printf "%s (%d)"

-- | Create a new `Error` for convenience
--
-- @since 0.1.0
new ::
     Maybe Error -- ^ The possible cause for the Error
  -> Code -- ^ The error code
  -> String -- ^ The description
  -> Error -- ^ The resulting Error
new c d e = Error {_nextCause = c, _code = d, _description = e}

-- | Append an error to the chain with a given description
--
-- @since 0.1.0
errAppend ::
     Error -- ^ The Error to which should be appended
  -> Code -- ^ The error code
  -> String -- ^ The description
  -> Error -- ^ The resulting Error
errAppend c = new $ Just c

-- | This is a convenient way to turn a string into an error value that can be
-- passed around, if you do not want to create a new Fail type for this use
-- case.
--
-- @since 0.1.0
err ::
     Code -- ^ The error code
  -> String -- ^ The description
  -> Error -- ^ The resulting Error
err = new Nothing

-- | The `Fail` implementation for the concrete `Error` type
--
-- @since 0.1.0
instance Fail Error where
  cause a = a ^. nextCause
  backtrace e@Error {_nextCause = Nothing} = [e]
  backtrace e@Error {_nextCause = Just c}  = e : backtrace c
